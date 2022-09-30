#include "include/Storage/DiskScanner.h"
#include <Storage/CustomDevice.h>
#include <debug_progmem.h>
#include "diskdefs.h"
#include <Data/Uuid.h>

namespace IFS
{
namespace FAT
{
#include "../fatfs/ff.h"
} // namespace FAT
} // namespace IFS

namespace Storage
{
namespace
{
using namespace diskdefs;

#define READ_SECTORS(buff, sector, count) device.read(sector << sectorSizeShift, buff, count << sectorSizeShift)
#define WRITE_SECTORS(buff, sector, count) device.write(sector << sectorSizeShift, buff, count << sectorSizeShift)

String getLabel(const char* s, unsigned length)
{
	while(length > 0 && s[length - 1] == 0x20) {
		--length;
	}
	return String(s, length);
}

// Convert unicode to OEM string
String unicode_to_oem(const uint16_t* str, size_t length)
{
	char buf[length];
	uint16_t c;
	auto out = buf;
	size_t outlen{0};
	while(length-- != 0 && (c = *str++)) {
		*out++ = IFS::FAT::ff_uni2oem(c, FF_CODE_PAGE);
		++outlen;
	}
	return String(buf, outlen);
}

DiskPart::Info* identify(Device& device, const WorkBuffer& buffer, storage_size_t offset)
{
	auto& fat = buffer.as<const FAT::fat_boot_sector_t>();
	auto& exfat = buffer.as<const EXFAT::boot_sector_t>();

	if(exfat.signature == MSDOS_MBR_SIGNATURE && exfat.fs_type == FSTYPE_EXFAT) {
		auto part = new DiskPart::Info(nullptr, Partition::SubType::Data::fat, offset,
									   exfat.vol_length << exfat.sect_size_bits, 0);
		part->systype = DiskPart::SysType::exfat;
		part->sectorSize = 1U << exfat.sect_size_bits;
		part->clusterSize = 1U << exfat.sect_size_bits << exfat.sect_per_clus_bits;
		// part->numFat = exfat.num_fats;
		debug_d("[DD] Found ExFAT @ 0x%llx", offset);
		return part;
	}

	// Valid JumpBoot code? (short jump, near jump or near call)
	auto b = fat.jmp_boot[0];
	if(b == 0xEB || b == 0xE9 || b == 0xE8) {
		if(fat.signature == MSDOS_MBR_SIGNATURE && fat.fat32.fs_type == FSTYPE_FAT32) {
			auto part = new DiskPart::Info(getLabel(fat.fat32.vol_label, MSDOS_NAME), Partition::SubType::Data::fat,
										   offset, (fat.sectors ?: fat.total_sect) * fat.sector_size, 0);
			part->systype = DiskPart::SysType::fat32;
			part->sectorSize = fat.sector_size;
			part->clusterSize = uint16_t(fat.sector_size * fat.sec_per_clus);
			// part->numFat = fat.num_fats;
			debug_d("[DD] Found FAT32 @ 0x%luu", offset);
			return part;
		}

		// FAT volumes formatted with early MS-DOS lack signature/fs_type
		auto w = fat.sector_size;
		b = fat.sec_per_clus;
		if((w & (w - 1)) == 0 && w >= 512 && w <= 4096			// Properness of sector size (512-4096 and 2^n)
		   && b != 0 && (b & (b - 1)) == 0						// Properness of cluster size (2^n)
		   && fat.reserved != 0									// Properness of reserved sectors (MNBZ)
		   && fat.num_fats - 1 <= 1								// Properness of FATs (1 or 2)
		   && fat.dir_entries != 0								// Properness of root dir entries (MNBZ)
		   && (fat.sectors >= 128 || fat.total_sect >= 0x10000) // Properness of volume sectors (>=128)
		   && fat.fat_length != 0) {							// Properness of FAT size (MNBZ)
			auto part = new DiskPart::Info(getLabel(fat.fat16.vol_label, MSDOS_NAME), Partition::SubType::Data::fat,
										   offset, (fat.sectors ?: fat.total_sect) * fat.sector_size, 0);
			part->clusterSize = uint16_t(fat.sector_size * fat.sec_per_clus);
			auto numClusters = part->size / part->clusterSize;
			part->systype = (numClusters <= MAX_FAT12) ? DiskPart::SysType::fat12 : DiskPart::SysType::fat16;
			part->sectorSize = fat.sector_size;
			// part->numFat = fat.num_fats;
			debug_d("[DD] Found FAT @ 0x%luu", offset);
			return part;
		}
	}

	return nullptr;
}

} // namespace

DiskScanner::DiskScanner(Device& device) : device(device)
{
}

DiskScanner::~DiskScanner()
{
}

unsigned DiskScanner::scanMbrEntries(uint32_t baseLba)
{
	auto& mbr = buffer.as<legacy_mbr_t>();
	unsigned n{0};
	for(unsigned i = 0; i < ARRAY_SIZE(mbr.partition_record); ++i) {
		auto& rec = mbr.partition_record[i];
		if(rec.starting_lba == 0 || rec.size_in_lba == 0) {
			continue;
		}
		mbrEntries[n] = rec;
		mbrEntries[n].starting_lba += baseLba;
		++n;
	}
	return n;
}

std::unique_ptr<DiskPart::Info> DiskScanner::next()
{
	if(state == State::idle) {
#if FF_MAX_SS != FF_MIN_SS
		sectorSize = device.getSectorSize();
		if(sectorSize > FF_MAX_SS || sectorSize < FF_MIN_SS || !isLog2(sectorSize)) {
			state = State::error;
			return nullptr;
		}
#else
		sectorSize = FF_MAX_SS;
#endif
		sectorSizeShift = getSizeBits(sectorSize);
		buffer = WorkBuffer(sectorSize, 1);
		if(!buffer) {
			return nullptr;
		}

		// Load sector 0 and check it
		if(!READ_SECTORS(buffer.get(), 0, 1)) {
			state = State::error;
			return nullptr;
		}

		auto part = identify(device, buffer, 0);
		if(part) {
			state = State::done;
			return std::unique_ptr<DiskPart::Info>(part);
		}

		/* Sector 0 is not an FAT VBR or forced partition number wants a partition */

		// GPT protective MBR?
		auto& mbr = buffer.as<legacy_mbr_t>();

		if(mbr.signature != MSDOS_MBR_SIGNATURE) {
			return nullptr;
		}

		if(mbr.partition_record[0].os_type == EFI_PMBR_OSTYPE_EFI_GPT) {
			// Load GPT header sector
			// if(!readSectors(buffer.get(), GPT_PRIMARY_PARTITION_TABLE_LBA, 1)) {
			if(!READ_SECTORS(buffer.get(), GPT_PRIMARY_PARTITION_TABLE_LBA, 1)) {
				debug_e("[DD] GPT header read failed");
				state = State::error;
				return nullptr;
			}
			auto& gpt = buffer.as<gpt_header_t>();
			if(!verifyGptHeader(gpt)) {
				debug_e("[DD] GPT invalid");
				state = State::error;
				return nullptr;
			}

			// Scan partition table
			numPartitionEntries = gpt.num_partition_entries;
			sector = gpt.partition_entry_lba;
			partitionIndex = 0;
			entryBuffer = WorkBuffer(sectorSize, 1);
			state = State::GPT;
		} else {
			mbrEntries.reset(new gpt_mbr_record_t[4]);
			numPartitionEntries = scanMbrEntries(0);
			state = State::MBR;
		}
	}

	while(partitionIndex < numPartitionEntries) {
		if(state == State::MBR) {
			auto& entry = mbrEntries[partitionIndex++];
			if(!READ_SECTORS(buffer.get(), entry.starting_lba, 1)) {
				continue;
			}
			if(entry.os_type == OSTYPE_EXTENDED) {
				numPartitionEntries = scanMbrEntries(entry.starting_lba);
				partitionIndex = 0;
				continue;
			}
			storage_size_t offset = entry.starting_lba << sectorSizeShift;
			auto part = identify(device, buffer, offset);
			if(part == nullptr) {
				part = new DiskPart::Info{};
				part->offset = offset;
				part->size = entry.size_in_lba << sectorSizeShift;
			}
			part->sysind = DiskPart::SysIndicator(entry.os_type);
			part->systype = DiskPart::getSysTypeFromIndicator(part->sysind);
			if(DiskPart::fatTypes[part->systype]) {
				part->type = Partition::Type::data;
				part->subtype = uint8_t(Partition::SubType::Data::fat);
			}
			return std::unique_ptr<DiskPart::Info>(part);
		}

		if(state == State::GPT) {
			auto entriesPerSector = sectorSize / sizeof(gpt_entry_t);
			if(partitionIndex % entriesPerSector == 0) {
				if(!READ_SECTORS(buffer.get(), sector++, 1)) {
					state = State::error;
					return nullptr;
				}
			}

			auto entries = buffer.as<gpt_entry_t[]>();
			auto& entry = entries[partitionIndex % entriesPerSector];
			++partitionIndex;
			if(!entry.partition_type_guid) {
				continue;
			}

			if(!READ_SECTORS(entryBuffer.get(), entry.starting_lba, 1)) {
				continue;
			}

			storage_size_t offset = entry.starting_lba << sectorSizeShift;
			auto part = identify(device, entryBuffer, offset);
			if(part == nullptr) {
				part = new DiskPart::Info{};
				part->offset = offset;
				part->size = (1 + entry.ending_lba - entry.starting_lba) << sectorSizeShift;
			}
			part->typeGuid = entry.partition_type_guid;
			part->uniqueGuid = entry.unique_partition_guid;
			part->name = unicode_to_oem(entry.partition_name, ARRAY_SIZE(entry.partition_name));
			return std::unique_ptr<DiskPart::Info>(part);
		}

		assert(false);
		state = State::error;
		return nullptr;
	}

	state = State::done;
	return nullptr;
}

bool scanDiskPartitions(Device& device)
{
	auto& dev = static_cast<CustomDevice&>(device);
	dev.clearPartitionTable();

	DiskScanner scanner(device);
	std::unique_ptr<DiskPart::Info> part;
	while((part = scanner.next())) {
		if(part->name.length() == 0 && part->uniqueGuid) {
			part->name = part->uniqueGuid;
		}
		dev.createPartition(part.release());
	}

	return true;
}

} // namespace Storage