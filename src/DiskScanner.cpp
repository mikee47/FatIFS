#include "include/Storage/DiskDevice.h"
#include <Storage/CustomDevice.h>
#include <debug_progmem.h>
#include "diskdefs.h"
#include "WorkBuffer.h"

namespace IFS
{
namespace FAT
{
#include "fatfs/ff.h"
} // namespace FAT
} // namespace IFS

namespace Storage
{
namespace
{
using namespace diskdefs;

enum class PartitionType {
	unknown,
	invalid,
	fat,
	fat32,
	exfat,
};

/* Find an FAT volume */
/* (It supports only generic partitioning rules, MBR, GPT and SFD) */

/*-----------------------------------------------------------------------*/
/* Load a sector and check if it is an FAT VBR                           */
/*-----------------------------------------------------------------------*/

String getLabel(const char* s, unsigned length)
{
	while(length > 0 && s[length - 1] == 0x20) {
		--length;
	}
	return String(s, length);
}

// Convert unicode to OEM string, in-place
String unicode_to_oem(const uint16_t* str, size_t length)
{
	String s;
	s.reserve(length);
	uint16_t c;
	auto out = s.begin();
	size_t outlen{0};
	while(length-- != 0 && (c = *str++)) {
		*out++ = IFS::FAT::ff_uni2oem(c, FF_CODE_PAGE);
		++outlen;
	}
	s.setLength(outlen);
	return s;
}

PartitionType identify(uint64_t offset, const WorkBuffer& buffer, Device& device, const gpt_entry* entry = nullptr)
{
	auto& dev = static_cast<CustomDevice&>(device);
	auto& fat = buffer.as<const FAT::fat_boot_sector>();
	auto& exfat = buffer.as<const EXFAT::boot_sector>();

	if(exfat.signature == MSDOS_MBR_SIGNATURE && exfat.fs_type == FSTYPE_EXFAT) {
		auto volumeSize = exfat.vol_length << exfat.sect_size_bits;
		String name;
		if(entry != nullptr) {
			name = unicode_to_oem(entry->partition_name, ARRAY_SIZE(entry->partition_name));
		}
		dev.createPartition(name, Partition::SubType::Data::exfat, offset, volumeSize);
		debug_d("[DD] Found ExFAT @ 0x%llx", offset);
		return PartitionType::exfat;
	}

	// Valid JumpBoot code? (short jump, near jump or near call)
	auto b = fat.jmp_boot[0];
	if(b == 0xEB || b == 0xE9 || b == 0xE8) {
		if(fat.signature == MSDOS_MBR_SIGNATURE && fat.fat32.fs_type == FSTYPE_FAT32) {
			auto volumeSize = (fat.sectors ?: fat.total_sect) * fat.sector_size;
			String label = getLabel(fat.fat32.vol_label, MSDOS_NAME);
			dev.createPartition(label, Partition::SubType::Data::fat32, offset, volumeSize);
			debug_d("[DD] Found FAT32 @ 0x%luu", offset);
			return PartitionType::fat32;
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
			auto volumeSize = (fat.sectors ?: fat.total_sect) * fat.sector_size;
			String label = getLabel(fat.fat16.vol_label, MSDOS_NAME);
			dev.createPartition(label, Partition::SubType::Data::fat, offset, volumeSize);
			debug_d("[DD] Found FAT @ 0x%luu", offset);
			return PartitionType::fat;
		}
	}

	return (fat.signature == MSDOS_MBR_SIGNATURE) ? PartitionType::unknown : PartitionType::invalid;
}

} // namespace

bool scanDiskPartitions(Device& device)
{
	constexpr auto sectorSize = SECTOR_SIZE;

	WorkBuffer buffer(sectorSize, 1);
	// Load sector 0 and check it
	if(!READ_SECTORS(buffer.get(), 0, 1)) {
		return false;
	}
	auto type = identify(0, buffer, device);
	if(type != PartitionType::unknown) {
		return true;
	}

	/* Sector 0 is not an FAT VBR or forced partition number wants a partition */

	// GPT protective MBR?
	auto& mbr = buffer.as<legacy_mbr>();
	if(mbr.partition_record[0].os_type == EFI_PMBR_OSTYPE_EFI_GPT) {
		// Load GPT header sector
		if(!READ_SECTORS(buffer.get(), GPT_PRIMARY_PARTITION_TABLE_LBA, 1)) {
			debug_e("[DD] GPT header read failed");
			return false;
		}
		auto& gpt = buffer.as<gpt_header>();
		if(!verifyGptHeader(gpt)) {
			debug_e("[DD] GPT invalid");
			return false;
		}

		// Scan partition table
		unsigned num_partition_entries = gpt.num_partition_entries;
		auto sect = gpt.partition_entry_lba;
		auto entriesPerSector = sectorSize / sizeof(gpt_entry);
		auto entries = buffer.as<gpt_entry[]>();
		for(unsigned i = 0; i < num_partition_entries; ++i) {
			if(i % entriesPerSector == 0) {
				if(!READ_SECTORS(buffer.get(), sect++, 1)) {
					break;
				}
			}

			auto& entry = entries[i % entriesPerSector];
			if(entry.partition_type_guid == PARTITION_BASIC_DATA_GUID) {
				WorkBuffer buffer(sectorSize, 1);
				if(READ_SECTORS(buffer.get(), entry.starting_lba, 1)) {
					identify(entry.starting_lba * sectorSize, buffer, device, &entry);
				}
			}
		}

		return true;
	}

	uint32_t partlba[4];
	for(unsigned i = 0; i < 4; ++i) {
		partlba[i] = mbr.partition_record[i].starting_lba;
	}
	for(auto sect : partlba) {
		if(sect != 0 && READ_SECTORS(buffer.get(), sect, 1)) {
			identify(sect * sectorSize, buffer, device);
		}
	}

	return true;
}

} // namespace Storage
