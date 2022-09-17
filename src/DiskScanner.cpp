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

bool identify(DiskPart& part, const WorkBuffer& buffer, uint64_t offset, const gpt_entry_t* entry = nullptr)
{
	auto& fat = buffer.as<const FAT::fat_boot_sector_t>();
	auto& exfat = buffer.as<const EXFAT::boot_sector_t>();

	if(exfat.signature == MSDOS_MBR_SIGNATURE && exfat.fs_type == FSTYPE_EXFAT) {
		part = DiskPart{
			.type = DiskPart::Type::exfat,
			.address = offset,
			.size = exfat.vol_length << exfat.sect_size_bits,
			.sectorSize = uint16_t(1 << exfat.sect_size_bits),
			.clusterSize = uint16_t(1 << exfat.sect_size_bits << exfat.sect_per_clus_bits),
			.numFat = exfat.num_fats,
		};
		if(entry != nullptr) {
			part.guid = Uuid(entry->unique_partition_guid);
			part.name = unicode_to_oem(entry->partition_name, ARRAY_SIZE(entry->partition_name));
		}
		debug_d("[DD] Found ExFAT @ 0x%llx", offset);
		return true;
	}

	// Valid JumpBoot code? (short jump, near jump or near call)
	auto b = fat.jmp_boot[0];
	if(b == 0xEB || b == 0xE9 || b == 0xE8) {
		if(fat.signature == MSDOS_MBR_SIGNATURE && fat.fat32.fs_type == FSTYPE_FAT32) {
			part = DiskPart{
				.type = DiskPart::Type::fat32,
				.address = offset,
				.size = (fat.sectors ?: fat.total_sect) * fat.sector_size,
				.name = getLabel(fat.fat32.vol_label, MSDOS_NAME),
				.sectorSize = fat.sector_size,
				.clusterSize = uint16_t(fat.sector_size * fat.sec_per_clus),
				.numFat = fat.num_fats,
			};
			debug_d("[DD] Found FAT32 @ 0x%luu", offset);
			return true;
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
			auto nclst = fat.sector_size * fat.sec_per_clus;
			part = DiskPart{
				.type = (nclst <= MAX_FAT12) ? DiskPart::Type::fat12 : DiskPart::Type::fat16,
				.address = offset,
				.size = (fat.sectors ?: fat.total_sect) * fat.sector_size,
				.name = getLabel(fat.fat16.vol_label, MSDOS_NAME),
				.sectorSize = fat.sector_size,
				.clusterSize = uint16_t(fat.sector_size * fat.sec_per_clus),
				.numFat = fat.num_fats,
			};
			debug_d("[DD] Found FAT @ 0x%luu", offset);
			return true;
		}
	}

	if(fat.signature == MSDOS_MBR_SIGNATURE) {
		part.type = DiskPart::Type::unknown;
		return false;
	}

	part.type = DiskPart::Type::invalid;
	return true;
}

} // namespace

bool DiskScanner::next(DiskPart& part)
{
	if(state == State::idle) {
#if FF_MAX_SS != FF_MIN_SS
		sectorSize = device.getSectorSize();
		if(sectorSize > FF_MAX_SS || sectorSize < FF_MIN_SS || (sectorSize & (sectorSize - 1))) {
			state = State::error;
			return false;
		}
#else
		sectorSize = FF_MAX_SS;
#endif
		buffer = WorkBuffer(sectorSize, 1);

		// Load sector 0 and check it
		if(!READ_SECTORS(buffer.get(), 0, 1)) {
			state = State::error;
			return false;
		}

		if(identify(part, buffer, 0)) {
			state = State::done;
			return true;
		}

		/* Sector 0 is not an FAT VBR or forced partition number wants a partition */

		// GPT protective MBR?
		auto& mbr = buffer.as<legacy_mbr_t>();
		if(mbr.partition_record[0].os_type == EFI_PMBR_OSTYPE_EFI_GPT) {
			// Load GPT header sector
			if(!READ_SECTORS(buffer.get(), GPT_PRIMARY_PARTITION_TABLE_LBA, 1)) {
				debug_e("[DD] GPT header read failed");
				state = State::error;
				return false;
			}
			auto& gpt = buffer.as<gpt_header_t>();
			if(!verifyGptHeader(gpt)) {
				debug_e("[DD] GPT invalid");
				state = State::error;
				return false;
			}

			// Scan partition table
			numPartitionEntries = gpt.num_partition_entries;
			sector = gpt.partition_entry_lba;
			partitionIndex = 0;
			entryBuffer = WorkBuffer(sectorSize, 1);
			state = State::GPT;
		} else {
			numPartitionEntries = ARRAY_SIZE(partlba);
			for(unsigned i = 0; i < numPartitionEntries; ++i) {
				partlba[i] = mbr.partition_record[i].starting_lba;
			}
			state = State::MBR;
		}
	}

	while(partitionIndex < numPartitionEntries) {
		if(state == State::MBR) {
			auto sect = partlba[partitionIndex++];
			if(sect == 0) {
				continue;
			}
			if(!READ_SECTORS(buffer.get(), sect, 1)) {
				continue;
			}
			identify(part, buffer, sect * sectorSize);
			return true;
		}

		if(state == State::GPT) {
			auto entriesPerSector = sectorSize / sizeof(gpt_entry_t);
			if(partitionIndex % entriesPerSector == 0) {
				if(!READ_SECTORS(buffer.get(), sector++, 1)) {
					state = State::error;
					return false;
				}
			}

			auto entries = buffer.as<gpt_entry_t[]>();
			auto& entry = entries[partitionIndex % entriesPerSector];
			++partitionIndex;
			if(entry.partition_type_guid != PARTITION_BASIC_DATA_GUID) {
				continue;
			}

			if(!READ_SECTORS(entryBuffer.get(), entry.starting_lba, 1)) {
				continue;
			}

			identify(part, entryBuffer, entry.starting_lba * sectorSize, &entry);
			return true;
		}

		assert(false);
		state = State::error;
		return false;
	}

	state = State::done;
	return false;
}

bool scanDiskPartitions(Device& device)
{
	auto& dev = static_cast<CustomDevice&>(device);
	DiskScanner scanner(device);
	DiskPart part;
	while(scanner.next(part)) {
		if(part.name.length() == 0) {
			part.name = part.guid;
		}
		switch(part.type) {
		case DiskPart::Type::fat12:
		case DiskPart::Type::fat16:
		case DiskPart::Type::fat32:
		case DiskPart::Type::exfat:
			dev.createPartition(part.name, Partition::SubType::Data::fat, part.address, part.size);
		default:; // Ignore
		}
	}

	return true;
}

} // namespace Storage
