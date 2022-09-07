#include "include/Storage/DiskDevice.h"
#include <Storage/CustomDevice.h>
#include <debug_progmem.h>

namespace IFS
{
namespace FAT
{
#include "fatfs/ff.h"
} // namespace FAT
} // namespace IFS

#define FSTYPE_FAT32 0x2020203233544146ULL // "FAT32   " 46 41 54 33 32 20 20 20
#define FSTYPE_EXFAT 0x2020205441465845ULL // "EXFAT   " 45 58 46 41 54 20 20 20

namespace Storage
{
namespace
{
namespace FAT
{
#include "msdos_fs.h"
}
namespace EXFAT
{
#include "exfat_raw.h"
}
#include "efi.h"

/* Find an FAT volume */
/* (It supports only generic partitioning rules, MBR, GPT and SFD) */

uint32_t crc32_byte(uint32_t crc, uint8_t d)
{
	crc ^= d;
	for(unsigned i = 0; i < 8; ++i) {
		uint32_t mask = -(crc & 1);
		crc = (crc >> 1) ^ (0xEDB88320 & mask);
	}
	return crc;
}

uint32_t crc32(const void* data, size_t length)
{
	auto ptr = static_cast<const uint8_t*>(data);
	uint32_t bcc = 0xFFFFFFFF;
	while(length-- != 0) {
		bcc = crc32_byte(bcc, *ptr++);
	}
	return ~bcc;
}

bool verifyGptHeader(gpt_header& gpt)
{
	/* Check signature, version (1.0) and length (92) */
	if(gpt.signature != GPT_HEADER_SIGNATURE) {
		return false;
	}
	if(gpt.revision != GPT_HEADER_REVISION_V1) {
		return false;
	}
	if(gpt.header_size != sizeof(gpt_header)) {
		return false;
	}

	uint32_t crc32_saved = gpt.header_crc32;
	gpt.header_crc32 = 0;
	uint32_t bcc = crc32(&gpt, sizeof(gpt));
	gpt.header_crc32 = crc32_saved;
	if(bcc != crc32_saved) {
		debug_e("[GPT] bcc 0x%08x, ~bcc 0x%08x, crc32 0x%08x", bcc, ~bcc, crc32_saved);
		return false;
	}
	if(gpt.sizeof_partition_entry != sizeof(gpt_entry)) {
		return false;
	}
	if(gpt.num_partition_entries > 128) {
		return false;
	}

	return true;
}

/*-----------------------------------------------------------------------*/
/* Load a sector and check if it is an FAT VBR                           */
/*-----------------------------------------------------------------------*/

enum class PartitionType {
	unknown,
	invalid,
	fat,
	fat32,
	exfat,
};

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

PartitionType identify(uint64_t offset, const void* sector, Device& device, const gpt_entry* entry = nullptr)
{
	auto& dev = static_cast<CustomDevice&>(device);
	auto fat = static_cast<const FAT::fat_boot_sector*>(sector);
	auto exfat = static_cast<const EXFAT::boot_sector*>(sector);

	if(exfat->signature == MSDOS_MBR_SIGNATURE && exfat->fs_type == FSTYPE_EXFAT) {
		auto volumeSize = exfat->vol_length << exfat->sect_size_bits;
		String name;
		if(entry != nullptr) {
			name = unicode_to_oem(entry->partition_name, ARRAY_SIZE(entry->partition_name));
		}
		dev.createPartition(name, Partition::SubType::Data::exfat, offset, volumeSize);
		debug_d("[DD] Found ExFAT @ 0x%llx", offset);
		return PartitionType::exfat;
	}

	// Valid JumpBoot code? (short jump, near jump or near call)
	auto b = exfat->jmp_boot[0];
	if(b == 0xEB || b == 0xE9 || b == 0xE8) {
		if(exfat->signature == MSDOS_MBR_SIGNATURE && fat->fat32.fs_type == FSTYPE_FAT32) {
			auto volumeSize = (fat->sectors ?: fat->total_sect) * fat->sector_size;
			String label = getLabel(fat->fat32.vol_label, sizeof(fat->fat32.vol_label));
			dev.createPartition(label, Partition::SubType::Data::fat32, offset, volumeSize);
			debug_d("[DD] Found FAT32 @ 0x%luu", offset);
			return PartitionType::fat32;
		}

		// FAT volumes formatted with early MS-DOS lack signature/fs_type
		auto w = fat->sector_size;
		b = fat->sec_per_clus;
		if((w & (w - 1)) == 0 && w >= 512 && w <= 4096			  /* Properness of sector size (512-4096 and 2^n) */
		   && b != 0 && (b & (b - 1)) == 0						  /* Properness of cluster size (2^n) */
		   && fat->reserved != 0								  /* Properness of reserved sectors (MNBZ) */
		   && fat->fats - 1 <= 1								  /* Properness of FATs (1 or 2) */
		   && fat->dir_entries != 0								  /* Properness of root dir entries (MNBZ) */
		   && (fat->sectors >= 128 || fat->total_sect >= 0x10000) /* Properness of volume sectors (>=128) */
		   && fat->fat_length != 0) {							  /* Properness of FAT size (MNBZ) */
			auto volumeSize = (fat->sectors ?: fat->total_sect) * fat->sector_size;
			String label = getLabel(fat->fat16.vol_label, sizeof(fat->fat16.vol_label));
			dev.createPartition(label, Partition::SubType::Data::fat, offset, volumeSize);
			debug_d("[DD] Found FAT @ 0x%luu", offset);
			return PartitionType::fat;
		}
	}

	return (exfat->signature == MSDOS_MBR_SIGNATURE) ? PartitionType::unknown : PartitionType::invalid;
}

} // namespace

bool scanDiskPartitions(Device& device)
{
	// Load sector 0 and check it
	uint8_t buffer[SECTOR_SIZE];
	if(!device.read(0, buffer, SECTOR_SIZE)) {
		return false;
	}
	auto type = identify(0, buffer, device);
	if(type != PartitionType::unknown) {
		return true;
	}

	/* Sector 0 is not an FAT VBR or forced partition number wants a partition */

	// GPT protective MBR?
	auto mbr = reinterpret_cast<legacy_mbr*>(buffer);
	if(mbr->partition_record[0].os_type == EFI_PMBR_OSTYPE_EFI_GPT) {
		// Load GPT header sector
		if(!device.read(GPT_PRIMARY_PARTITION_TABLE_LBA * SECTOR_SIZE, buffer, SECTOR_SIZE)) {
			debug_e("[DD] GPT header read failed");
			return false;
		}
		auto gpt = reinterpret_cast<gpt_header*>(buffer);
		if(!verifyGptHeader(*gpt)) {
			debug_e("[DD] GPT invalid");
			return false;
		}

		// Scan partition table
		efi_guid_t basic = PARTITION_BASIC_DATA_GUID;
		unsigned num_partition_entries = gpt->num_partition_entries;
		uint64_t gptEntryOffset = gpt->partition_entry_lba * SECTOR_SIZE;
		for(unsigned i = 0; i < num_partition_entries; i++) {
			if(gptEntryOffset % SECTOR_SIZE == 0) {
				if(!device.read(gptEntryOffset, buffer, SECTOR_SIZE)) {
					break;
				}
			}

			auto entry = reinterpret_cast<gpt_entry*>(&buffer[gptEntryOffset % SECTOR_SIZE]);
			if(entry->partition_type_guid == basic) {
				uint8_t buffer[SECTOR_SIZE];
				auto offset = entry->starting_lba * SECTOR_SIZE;
				if(device.read(offset, &buffer, SECTOR_SIZE)) {
					identify(offset, buffer, device, entry);
				}
			}

			gptEntryOffset += sizeof(gpt_entry);
		}

		return true;
	}

	uint32_t partlba[4];
	for(unsigned i = 0; i < 4; ++i) {
		partlba[i] = mbr->partition_record[i].starting_lba;
	}
	for(unsigned i = 0; i < 4; ++i) {
		uint64_t offset = partlba[i] * SECTOR_SIZE;
		if(offset != 0 && device.read(offset, buffer, SECTOR_SIZE)) {
			identify(offset, buffer, device);
		}
	}

	return true;
}

} // namespace Storage
