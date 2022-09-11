#include "include/Storage/DiskDevice.h"
#include <Storage/CustomDevice.h>
#include <debug_progmem.h>

extern "C" int os_get_random(void* buf, size_t len);

namespace IFS
{
namespace FAT
{
#include "fatfs/ff.h"
#include "fatfs/diskio.h"
} // namespace FAT
} // namespace IFS

#define FSTYPE_FAT 0x2020202020544146ULL   // "FAT     " 46 41 54 20 20 20 20 20
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

template <typename T> T align_up(T value, uint32_t align)
{
	return (value + align - 1) & ~(T(align) - 1);
}

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

uint32_t crc32(uint32_t bcc, const void* data, size_t length)
{
	bcc = ~bcc;
	auto ptr = static_cast<const uint8_t*>(data);
	while(length-- != 0) {
		bcc = crc32_byte(bcc, *ptr++);
	}
	return ~bcc;
}

uint32_t crc32(const void* data, size_t length)
{
	return crc32(0, data, length);
}

template <typename T> getBlockCount(T byteCount, uint32_t blockSize)
{
	return (byteCount + blockSize - 1) / blockSize;
}

// exFAT checksumming
uint32_t xsum32(uint8_t dat, uint32_t sum)
{
	return ((sum << 31) | (sum >> 1)) + dat;
}

uint32_t xsum32(const void* buffer, size_t length, uint32_t sum)
{
	auto pb = static_cast<const uint8_t*>(buffer);
	while(length-- != 0) {
		sum = xsum32(*pb++, sum);
	}
	return sum;
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
	auto& mbr = *reinterpret_cast<legacy_mbr*>(buffer);
	if(mbr.partition_record[0].os_type == EFI_PMBR_OSTYPE_EFI_GPT) {
		// Load GPT header sector
		if(!device.read(GPT_PRIMARY_PARTITION_TABLE_LBA * SECTOR_SIZE, buffer, SECTOR_SIZE)) {
			debug_e("[DD] GPT header read failed");
			return false;
		}
		auto& gpt = *reinterpret_cast<gpt_header*>(buffer);
		if(!verifyGptHeader(gpt)) {
			debug_e("[DD] GPT invalid");
			return false;
		}

		// Scan partition table
		unsigned num_partition_entries = gpt.num_partition_entries;
		uint64_t gptEntryOffset = gpt.partition_entry_lba * SECTOR_SIZE;
		for(unsigned i = 0; i < num_partition_entries; i++) {
			if(gptEntryOffset % SECTOR_SIZE == 0) {
				if(!device.read(gptEntryOffset, buffer, SECTOR_SIZE)) {
					break;
				}
			}

			auto& entry = *reinterpret_cast<gpt_entry*>(&buffer[gptEntryOffset % SECTOR_SIZE]);
			if(entry.partition_type_guid == PARTITION_BASIC_DATA_GUID) {
				uint8_t buffer[SECTOR_SIZE];
				auto offset = entry.starting_lba * SECTOR_SIZE;
				if(device.read(offset, &buffer, SECTOR_SIZE)) {
					identify(offset, buffer, device, &entry);
				}
			}

			gptEntryOffset += sizeof(gpt_entry);
		}

		return true;
	}

	uint32_t partlba[4];
	for(unsigned i = 0; i < 4; ++i) {
		partlba[i] = mbr.partition_record[i].starting_lba;
	}
	for(unsigned i = 0; i < 4; ++i) {
		uint64_t offset = partlba[i] * SECTOR_SIZE;
		if(offset != 0 && device.read(offset, buffer, SECTOR_SIZE)) {
			identify(offset, buffer, device);
		}
	}

	return true;
}

/*-----------------------------------------------------------------------*/
/* Create FAT/exFAT volume (with sub-functions)                          */
/*-----------------------------------------------------------------------*/

#define N_SEC_TRACK 63	 // Sectors per track for determination of drive CHS
#define GPT_ALIGN 0x100000 // Alignment of partitions in GPT [byte] (>=128KB)
#define GPT_ITEMS 128	  // Number of GPT table size (>=128, sector aligned)

using namespace IFS::FAT;

int disk_write(uint8_t pdrv, const void* buff, LBA_t sector, size_t count)
{
	return 0;
}

/* Create partitions on the physical drive in format of MBR or GPT */
static FRESULT create_partition(uint8_t drv,		// Physical drive number
								const LBA_t plst[], // Partition list
								uint8_t sys,		// System ID (for only MBR, temp setting)
								uint8_t* buf		// Working buffer for a sector
)
{
	/* Get physical drive size */
	LBA_t driveSectors;
	// if(disk_ioctl(drv, GET_SECTOR_COUNT, &driveSectors) != RES_OK) {
	// 	return FR_DISK_ERR;
	// }

#if FF_LBA64
	if(driveSectors >= FF_MIN_GPT) {
		/* Create partitions in GPT format */

		uint16_t sectorSize;
#if FF_MAX_SS == FF_MIN_SS
		sectorSize = FF_MAX_SS;
#else
		if(disk_ioctl(drv, GET_SECTOR_SIZE, &sectorSize) != RES_OK) {
			return FR_DISK_ERR; /* Get sector size */
		}
		if(sectorSize > FF_MAX_SS || sectorSize < FF_MIN_SS || (sectorSize & (sectorSize - 1))) {
			return FR_DISK_ERR;
		}
#endif
		uint32_t align = GPT_ALIGN / sectorSize;						 // Partition alignment for GPT [sector]
		unsigned ptSectors = GPT_ITEMS * sizeof(gpt_entry) / sectorSize; // Size of partition table [sector]
		uint64_t top_bpt = driveSectors - ptSectors - 1;				 // Backup partiiton table start sector
		uint64_t nxt_alloc = 2 + ptSectors;								 // First allocatable sector
		uint64_t sz_pool = top_bpt - nxt_alloc;							 // Size of allocatable area
		uint32_t bcc = 0;												 // Cumulative partition entry checksum
		uint64_t sz_part = 1;
		unsigned ptIndex = 0; // partition table index
		unsigned si = 0;	  // size table index */
		do {
			unsigned ofs = ptIndex * sizeof(gpt_entry) % sectorSize;
			if(ofs == 0) {
				// Clean the buffer if needed
				memset(buf, 0, sectorSize);
			}

			// Is the size table not terminated?
			if(sz_part != 0) {
				// Align partition start
				nxt_alloc = align_up(nxt_alloc, align);
				sz_part = plst[si++]; // Get a partition size
				// Is the size in percentage?
				if(sz_part <= 100) {
					sz_part = sz_pool * sz_part / 100;
					// Align partition end
					sz_part = align_up(sz_part, align);
				}
				// Clip the size at end of the pool
				if(nxt_alloc + sz_part > top_bpt) {
					sz_part = (nxt_alloc < top_bpt) ? top_bpt - nxt_alloc : 0;
				}
			}

			// Add a partition?
			if(sz_part != 0) {
				auto entry = reinterpret_cast<gpt_entry*>(&buf[ofs]);
				entry->partition_type_guid = PARTITION_BASIC_DATA_GUID;
				os_get_random(&entry->unique_partition_guid, sizeof(efi_guid_t));
				entry->starting_lba = nxt_alloc;
				entry->ending_lba = nxt_alloc + sz_part - 1;
				nxt_alloc += sz_part; // Next allocatable sector
			}

			// Write the buffer if it is filled up
			if((ptIndex + 1) * sizeof(gpt_entry) % sectorSize == 0) {
				// Update cumulative partition entry checksum
				bcc = crc32(bcc, buf, sectorSize);
				// Write to primary table
				auto entryRelativeSector = ptIndex * sizeof(gpt_entry) / sectorSize;
				if(disk_write(drv, buf, 2 + entryRelativeSector, 1) != RES_OK) {
					return FR_DISK_ERR;
				}
				// Write to secondary table
				if(disk_write(drv, buf, top_bpt + entryRelativeSector, 1) != RES_OK) {
					return FR_DISK_ERR;
				}
			}
		} while(++ptIndex < GPT_ITEMS);

		/* Create primary GPT header */
		auto& header = *reinterpret_cast<gpt_header*>(buf);
		header = {
			.signature = GPT_HEADER_SIGNATURE,
			.revision = GPT_HEADER_REVISION_V1,
			.header_size = sizeof(gpt_header),
			.my_lba = 1,
			.alternate_lba = driveSectors - 1,
			.first_usable_lba = 2 + ptSectors,
			.last_usable_lba = top_bpt - 1,
			.partition_entry_lba = 2,
			.num_partition_entries = GPT_ITEMS,
			.sizeof_partition_entry = sizeof(gpt_entry),
			.partition_entry_array_crc32 = bcc,
		};
		os_get_random(&header.disk_guid, sizeof(efi_guid_t));
		header.header_crc32 = crc32(&header, sizeof(header));
		if(disk_write(drv, buf, header.my_lba, 1) != RES_OK) {
			return FR_DISK_ERR;
		}

		/* Create secondary GPT header */
		std::swap(header.my_lba, header.alternate_lba);
		header.partition_entry_lba = top_bpt;
		header.header_crc32 = 0;
		header.header_crc32 = crc32(&header, sizeof(header));
		if(disk_write(drv, buf, header.my_lba, 1) != RES_OK) {
			return FR_DISK_ERR;
		}

		/* Create protective MBR */
		auto& mbr = *reinterpret_cast<legacy_mbr*>(buf);
		mbr = {
			.signature = MSDOS_MBR_SIGNATURE,
		};
		static const gpt_mbr_record gpt_mbr PROGMEM = {
			.boot_indicator = 0,
			.start_head = 0,
			.start_sector = 2,
			.start_track = 0,
			.os_type = EFI_PMBR_OSTYPE_EFI_GPT,
			.end_head = 0xfe,
			.end_sector = 0xff,
			.end_track = 0,
			.starting_lba = 1,
			.size_in_lba = 0xffffffff,
		};
		memcpy_P(&mbr.partition_record[0], &gpt_mbr, sizeof(gpt_mbr));
		if(disk_write(drv, buf, 0, 1) != RES_OK) {
			return FR_DISK_ERR;
		}
	} else
#endif
	{
		/* Create partitions in MBR format */

		uint32_t sz_drv32 = driveSectors;
		// Determine drive CHS without any consideration of the drive geometry
		uint8_t n_sc = N_SEC_TRACK;
		uint8_t n_hd;
		for(n_hd = 8; n_hd != 0 && sz_drv32 / (n_hd * n_sc) > 1024; n_hd *= 2) {
		}
		if(n_hd == 0) {
			// Number of heads needs to be < 256
			n_hd = 255;
		}

		memset(buf, 0, FF_MAX_SS); // Clear MBR
		auto& mbr = *reinterpret_cast<legacy_mbr*>(buf);

		unsigned i;
		uint32_t nxt_alloc32;
		for(i = 0, nxt_alloc32 = n_sc; i < 4 && nxt_alloc32 != 0 && nxt_alloc32 < sz_drv32; ++i) {
			uint32_t sz_part32 = plst[i]; /* Get partition size */
			if(sz_part32 <= 100) {
				// Size in percentage
				sz_part32 = (sz_part32 == 100) ? sz_drv32 : sz_drv32 / 100 * sz_part32;
			}
			if(nxt_alloc32 + sz_part32 > sz_drv32 || nxt_alloc32 + sz_part32 < nxt_alloc32) {
				// Clip at drive size
				sz_part32 = sz_drv32 - nxt_alloc32;
			}
			if(sz_part32 == 0) {
				// End of table or no sector to allocate
				break;
			}

			auto pte = &mbr.partition_record[i];

			pte->starting_lba = nxt_alloc32;
			pte->size_in_lba = sz_part32;
			pte->os_type = sys;

			// Start CHS
			unsigned cy = nxt_alloc32 / n_sc / n_hd;	 // cylinder
			pte->start_head = nxt_alloc32 / n_sc % n_hd; // head
			uint8_t sc = nxt_alloc32 % n_sc + 1;		 // sector
			pte->start_sector = (cy >> 2 & 0xC0) | sc;
			pte->start_track = cy;
			// End CHS
			cy = (nxt_alloc32 + sz_part32 - 1) / n_sc / n_hd;			 // cylinder
			pte->end_head = (nxt_alloc32 + sz_part32 - 1) / n_sc % n_hd; // head
			sc = (nxt_alloc32 + sz_part32 - 1) % n_sc + 1;				 // sector
			pte->end_sector = (cy >> 2 & 0xC0) | sc;
			pte->end_track = cy;

			nxt_alloc32 += sz_part32;
		}

		mbr.signature = MSDOS_MBR_SIGNATURE;
		// Write the MBR
		if(disk_write(drv, buf, 0, 1) != RES_OK) {
			return FR_DISK_ERR;
		}
	}

	return FR_OK;
}

FRESULT f_mkfs(const MKFS_PARM* opt, // Format options
			   void* workBuffer,	 // Pointer to working buffer (null: use heap memory)
			   size_t workBufferSize // Size of working buffer [byte]
)
{
	static const uint16_t cst[] = {1, 4, 16, 64, 256, 512, 0}; /* Cluster size boundary for FAT volume (4Ks unit) */
	static const uint16_t cst32[] = {1, 2, 4, 8, 16, 32, 0};   /* Cluster size boundary for FAT32 volume (128Ks unit) */
	uint8_t fsty, sys, *pte, pdrv, ipart;
	uint32_t n_clst, nsect, n, vsn;
	LBA_t sz_vol, b_vol, b_data; /* Size of volume, Base LBA of volume, fat, data */
	LBA_t sect, lba[2];
	uint32_t sz_rsv, sz_dir; /* Size of reserved, fat, dir */
	unsigned i;
	FRESULT fr;

	/* Check mounted drive and clear work area */
	static const MKFS_PARM defopt = {FM_ANY};
	if(opt == nullptr) {
		// Use default parameter if it is not given
		opt = &defopt;
	}

	/* Get physical drive status (sz_drv, sz_blk, sectorSize) */
	uint32_t sz_blk = opt->align;
	if(sz_blk == 0 && disk_ioctl(pdrv, GET_BLOCK_SIZE, &sz_blk) != RES_OK) {
		sz_blk = 1;
	}
	if(sz_blk == 0 || sz_blk > 0x8000 || (sz_blk & (sz_blk - 1))) {
		sz_blk = 1;
	}

	uint16_t sectorSize;
#if FF_MAX_SS != FF_MIN_SS
	if(disk_ioctl(pdrv, GET_SECTOR_SIZE, &sectorSize) != RES_OK) {
		return FR_DISK_ERR;
	}
	if(sectorSize > FF_MAX_SS || sectorSize < FF_MIN_SS || (sectorSize & (sectorSize - 1))) {
		return FR_DISK_ERR;
	}
#else
	sectorSize = FF_MAX_SS;
#endif
	/* Options for FAT sub-type and FAT parameters */
	uint8_t fsopt = opt->fmt & (FM_ANY | FM_SFD);
	unsigned n_fat = (opt->n_fat >= 1 && opt->n_fat <= 2) ? opt->n_fat : 1;
	unsigned n_root = opt->n_root;
	if(n_root == 0 || n_root > 32768 || (n_root % (sectorSize / sizeof(exfat_dentry))) != 0) {
		nroot = 512;
	}
	uint32_t sz_au = (opt->au_size <= 0x1000000 && (opt->au_size & (opt->au_size - 1)) == 0) ? opt->au_size : 0;
	sz_au /= sectorSize; // Byte --> Sector

	/* Get working buffer */
	uint32_t sz_buf = workBufferSize / sectorSize; // Size of working buffer [sector]
	if(sz_buf == 0) {
		return FR_NOT_ENOUGH_CORE;
	}
	auto buf = static_cast<uint8_t*>(workBuffer);
#if FF_USE_LFN == 3
	if(!buf) {
		buf = ff_memalloc(sz_buf * sectorSize); /* Use heap memory for working buffer */
	}
#endif
	if(buf == nullptr) {
		return FR_NOT_ENOUGH_CORE;
	}

	/* Determine where the volume to be located (b_vol, sz_vol) */
	LBA_t b_vol = 0;
	LBA_t sz_vol = 0;
	if(FF_MULTI_PARTITION && ipart != 0) { /* Is the volume associated with any specific partition? */
		/* Get partition location from the existing partition table */

		// Load MBR
		if(disk_read(pdrv, workBuffer, 0, 1) != RES_OK) {
			return FR_DISK_ERR;
		}
		auto& mbr = static_cast<legacy_mbr*>(workBuffer);
		if(mbr.signature != MSDOS_MBR_SIGNATURE) {
			return FR_MKFS_ABORTED;
		}
#if FF_LBA64
		// GPT protective MBR?
		if(mbr->partition_record[0].os_type == EFI_PMBR_OSTYPE_EFI_GPT) {
			/* Get the partition location from GPT */
			if(disk_read(pdrv, workBuffer, 1, 1) != RES_OK) {
				return (FR_DISK_ERR);
			}
			auto& gpt = *static_cast<gpt_header*>(workBuffer);
			if(!verifyGptHeader(gpt)) {
				return FR_MKFS_ABORTED;
			}
			auto n_ent = gpt.num_partition_entries;
			auto pt_lba = gpt.partition_entry_lba;
			uint32_t ofs = 0;
			unsigned i = 0;
			auto entry = static_cast<gpt_entry*>(workBuffer);
			/* Find MS Basic partition with order of ipart */
			for(unsigned offset = 0; n_ent != 0; --n_ent, ++entry, offset += sizeof(gpt_entry)) {
				if(offset % sectorSize == 0) {
					if(disk_read(pdrv, workBuffer, pt_lba++, 1) != RES_OK) {
						return FR_DISK_ERR;
					}
					offset = 0;
					entry = static_cast<gpt_entry*>(workBuffer);
				}
				if(entry->partition_type_guid == PARTITION_BASIC_DATA_GUID && ++i == ipart) {
					b_vol = entry->starting_lba;
					sz_vol = 1 + entry->ending_lba - entry->starting_lba;
					break;
				}
			}
			if(n_ent == 0) {
				// Partition not found
				return FR_MKFS_ABORTED;
			}
			// Partitioning is in GPT
			fsopt |= 0x80;
		} else
#endif
		{
			/* Get the partition location from MBR partition table */
			auto& entry = mbr->partition_record[ipart - 1];
			if(ipart > 4 || entry.os_type == 0) {
				return FR_MKFS_ABORTED; // No partition
			}
			b_vol = entry.starting_lba;
			sz_vol = entry.size_in_lba;
		}
	} else {
		/* The volume is associated with a physical drive */
		if(disk_ioctl(pdrv, GET_SECTOR_COUNT, &sz_vol) != RES_OK) {
			return FR_DISK_ERR;
		}
		// To be partitioned?
		if(!(fsopt & FM_SFD)) {
			// Create a single-partition on the drive
#if FF_LBA64
			// Decide on MBR/GPT partition type
			if(sz_vol >= FF_MIN_GPT) {
				// Partitioning is in GPT
				fsopt |= 0x80;
				b_vol = GPT_ALIGN / sectorSize;
				// Estimate partition offset and size
				sz_vol -= 1 + b_vol + GPT_ITEMS * sizeof(gpt_entry) / sectorSize;
			} else
#endif
			{
				// Partitioning is in MBR
				if(sz_vol > N_SEC_TRACK) {
					// Estimate partition offset and size
					b_vol = N_SEC_TRACK;
					sz_vol -= b_vol;
				}
			}
		}
	}
	// Check for minimum volume size
	if(sz_vol < 128) {
		return FR_MKFS_ABORTED;
	}

	/* Now start to create a FAT volume at b_vol and sz_vol */

	do {
		/* Pre-determine the FAT type */
#if FF_FS_EXFAT
		if(&FM_EXFAT) {
			// exFAT only, vol >= 64MS or sz_au > 128S ?
			if((fsopt & FM_ANY) == FM_EXFAT || sz_vol >= 0x4000000 || sz_au > 128) {
				fsty = FS_EXFAT;
				break;
			}
		}
#endif
#if FF_LBA64
		// FAT32 volumes limited to 4GB
		if(sz_vol >> 32 != 0) {
			return FR_MKFS_ABORTED;
		}
#endif
		// Ensure AU is valid for FAT/FAT32
		if(sz_au > 128) {
			sz_au = 128;
		}
		if((fsopt & FM_ANY) == FM_FAT32) {
			fsty = FS_FAT32;
			break;
		}
		if(!(fsopt & FM_FAT)) {
			return FR_INVALID_PARAMETER;
		}
		fsty = FS_FAT16;
	} while(0);

	vsn = (uint32_t)sz_vol + GET_FATTIME(); /* VSN generated from current time and partitiion size */

#if FF_FS_EXFAT
	if(fsty == FS_EXFAT) {
		/* Create an exFAT volume */
		uint32_t nbit, clu;
		WCHAR ch, si;

		if(sz_vol < 0x1000) {
			// Volume too small for exFAT
			return FR_MKFS_ABORTED;
		}
#if FF_USE_TRIM
		// Inform storage device that the volume area may be erased
		lba[0] = b_vol;
		lba[1] = b_vol + sz_vol - 1;
		disk_ioctl(pdrv, CTRL_TRIM, lba);
#endif
		/* Determine FAT location, data location and number of clusters */
		if(sz_au == 0) { /* AU auto-selection */
			if(sz_vol >= 0x4000000) {
				sz_au = 256; /* >= 64Ms */
			} else if(sz_vol >= 0x80000) {
				sz_au = 64; /* >= 512Ks */
			} else {
				sz_au = 8;
			}
		}
		LBA_t b_fat = b_vol + 32;													// FAT start at offset 32
		uint32_t sz_fat = ((sz_vol / sz_au + 2) * 4 + sectorSize - 1) / sectorSize; // Number of FAT sectors
		LBA_t b_data = align_up(b_fat + sz_fat, sz_blk); // Align data area to the erase block boundary
		if(b_data - b_vol >= sz_vol / 2) {
			// Volume too small
			return FR_MKFS_ABORTED;
		}
		uint32_t n_clst = (sz_vol - (b_data - b_vol)) / sz_au; // Number of clusters
		if(n_clst < 16 || n_clst > MAX_EXFAT) {
			// Too few/many clusters
			return FR_MKFS_ABORTED;
		}

		uint32_t szb_bit = (n_clst + 7) / 8; // Size of allocation bitmap
		uint32_t clen[3] = {
			(szb_bit + sz_au * sectorSize - 1) / (sz_au * sectorSize), // Number of allocation bitmap clusters
		};

		/* Create a compressed up-case table */
		LBA_t sect = b_data + sz_au * clen[0]; // Table start sector
		uint32_t sum = 0;					   // Table checksum to be stored in the 82 entry
		unsigned st = 0;
		si = 0;
		i = 0;
		unsigned j = 0;
		uint32_t szb_case = 0;
		do {
			switch(st) {
			case 0:
				WCHAR ch = ff_wtoupper(si);
				if(ch != si) {
					// Store the up-case char if exist
					++si;
					break;
				}
				// Get run length of no-case block
				WCHAR wc = si + 1;
				while(wc != 0 && wc == ff_wtoupper(wc)) {
					++wc;
				}
				j = wc - si;
				// Compress the no-case block if run is >= 128 chars
				if(j >= 128) {
					ch = 0xFFFF;
					st = 2;
					break;
				}
				// Do not compress short run
				st = 1;
				/* FALLTHROUGH */

			case 1:
				// Fill the short run
				ch = si++;
				if(--j == 0) {
					st = 0;
				}
				break;

			default:
				ch = j;
				si += j; // Number of chars to skip
				st = 0;
			}

			/* Put it into the write buffer */
			sum = xsum32(buf[i + 0] = ch, sum);
			sum = xsum32(buf[i + 1] = (ch >> 8), sum);
			i += 2;
			szb_case += 2;

			// Write buffered data when buffer full or end of process
			if(si == 0 || i == sz_buf * sectorSize) {
				n = (i + sectorSize - 1) / sectorSize;
				if(disk_write(pdrv, buf, sect, n) != RES_OK) {
					return FR_DISK_ERR;
				}
				sect += n;
				i = 0;
			}
		} while(si != 0);
		clen[1] = getBlockCount(szb_case, sz_au * sectorSize); // Number of up-case table clusters
		clen[2] = 1;										   // Number of root dir clusters

		/* Initialize the allocation bitmap */
		sect = b_data;
		nsect = getBlockCount(szb_bit, sectorSize); // Start of bitmap and number of bitmap sectors
		nbit = clen[0] + clen[1] + clen[2]; // Number of clusters in-use by system (bitmap, up-case and root-dir)
		do {
			memset(buf, 0, sz_buf * sectorSize); /* Initialize bitmap buffer */
			for(i = 0; nbit != 0 && i / 8 < sz_buf * sectorSize; buf[i / 8] |= 1 << (i % 8), i++, nbit--) {
				; /* Mark used clusters */
			}
			n = (nsect > sz_buf) ? sz_buf : nsect; /* Write the buffered data */
			if(disk_write(pdrv, buf, sect, n) != RES_OK) {
				return FR_DISK_ERR;
			}
			sect += n;
			nsect -= n;
		} while(nsect != 0);

		/* Initialize the FAT */
		sect = b_fat;
		nsect = sz_fat; /* Start of FAT and number of FAT sectors */
		j = nbit = clu = 0;
		do {
			memset(buf, 0, sz_buf * sectorSize);
			i = 0;		   /* Clear work area and reset write offset */
			if(clu == 0) { /* Initialize FAT [0] and FAT[1] */
				st_dword(buf + i, 0xFFFFFFF8);
				i += 4;
				clu++;
				st_dword(buf + i, 0xFFFFFFFF);
				i += 4;
				clu++;
			}
			do {											  /* Create chains of bitmap, up-case and root dir */
				while(nbit != 0 && i < sz_buf * sectorSize) { /* Create a chain */
					st_dword(buf + i, (nbit > 1) ? clu + 1 : 0xFFFFFFFF);
					i += 4;
					clu++;
					nbit--;
				}
				if(nbit == 0 && j < 3) {
					nbit = clen[j++]; /* Get next chain length */
				}
			} while(nbit != 0 && i < sz_buf * sectorSize);
			n = (nsect > sz_buf) ? sz_buf : nsect; /* Write the buffered data */
			if(disk_write(pdrv, buf, sect, n) != RES_OK) {
				return FR_DISK_ERR;
			}
			sect += n;
			nsect -= n;
		} while(nsect != 0);

		/* Initialize the root directory */
		memset(buf, 0, sz_buf * sectorSize);
		auto dir = static_cast<exfat_dentry*>(workBuffer);
		// Volume label entry (no label)
		dir[0].type = ET_VLABEL;
		// Bitmap entry
		dir[1].type = ET_BITMAP;
		dir[1].bitmap.start_clu = 2;
		dir[1].bitmap.size = szb_bit;
		// Up-case table entry
		dir[2].type = ET_UPCASE;
		dir[2].upcase.checksum = sum;
		dir[2].upcase.start_clu = 2 + clen[0];
		dir[2].upcase.size = szb_case;

		sect = b_data + sz_au * (clen[0] + clen[1]);
		nsect = sz_au; /* Start of the root directory and number of sectors */
		do {		   /* Fill root directory sectors */
			n = (nsect > sz_buf) ? sz_buf : nsect;
			if(disk_write(pdrv, buf, sect, n) != RES_OK) {
				return FR_DISK_ERR;
			}
			memset(buf, 0, sectorSize); // Rest of entries are filled with zero
			sect += n;
			nsect -= n;
		} while(nsect != 0);

		/* Create two sets of the exFAT VBR blocks */
		sect = b_vol;
		for(n = 0; n < 2; n++) {
			/* Main record (+0) */
			memset(buf, 0, sectorSize);
			auto& bpb = *static_cast<EXFAT::boot_sector*>(workBuffer);
			*bpb = {
				.jmp_boot = {0xEB, 0x76, 0x90},
				.fs_type = FSTYPE_EXFAT,
				.partition_offset = b_vol,			   // Volume offset in the physical drive [sector]
				.vol_length = sz_vol,				   // Volume size [sector]
				.fat_offset = b_fat - b_vol,		   // FAT offset [sector]
				.fat_length = sz_fat,				   // FAT size [sector]
				.clu_offset = b_data - b_vol,		   // Data offset [sector]
				.clu_count = n_clst,				   // Number of clusters
				.root_cluster = 2 + clen[0] + clen[1], // Root dir cluster #
				.vol_serial = vsn,
				.fs_revision = 0x0100, // Filesystem version (1.00)
				.sect_size_bits = __builtin_ffs(sectorSize) - 1,
				.sect_per_clus_bits = __builtin_ffs(sz_au) - 1,
				.num_fats = 1,
				.drv_sel = 0x80,		   // Drive number (for int13)
				.boot_code = {0xEB, 0xFE}, // Boot code (x86)
				.signature = BOOT_SIGNATURE,
			};

			// Calculate VBR checksum
			// NOTE: vol_flags and percent_in_use NOT included
			sum = 0;
			for(i = 0; i < sectorSize; ++i) {
				if(i != BPB_VolFlagEx && i != BPB_VolFlagEx + 1 && i != BPB_PercInUseEx)
					sum = xsum32(buf[i], sum);
			}
			if(disk_write(pdrv, buf, sect++, 1) != RES_OK) {
				return FR_DISK_ERR;
			}
			/* Extended bootstrap record (+1..+8) */
			memset(bpb, 0, sectorSize);
			bpb.signature = BOOT_SIGNATURE;
			for(j = 1; j < 9; ++j) {
				sum = xsum32(buf, sectorSize, sum);
				if(disk_write(pdrv, buf, sect++, 1) != RES_OK) {
					return (FR_DISK_ERR);
				}
			}
			/* OEM/Reserved record (+9..+10) */
			memset(buf, 0, sectorSize);
			for(; j < 11; j++) {
				sum = xsum32(buf, sectorSize, sum);
				if(disk_write(pdrv, buf, sect++, 1) != RES_OK)
					return (FR_DISK_ERR);
			}
			/* Fill sum record (+11) with checksum value */
			std::fill_n(static_cast<uint32_t*>(workBuffer), sectorSize / 4, sum);
			if(disk_write(pdrv, buf, sect++, 1) != RES_OK) {
				return FR_DISK_ERR;
			}
		}

	} else
#endif /* FF_FS_EXFAT */
	{
		/* Create a FAT/FAT32 volume */
		do {
			uint32_t pau = sz_au;
			uint32_t n_clst;
			uint32_t sz_fat; // FAT size [sector]
			uint32_t sz_rsv; // Number of reserved sectors
			uint32_t sz_dir; // Root dir size [sector]
			/* Pre-determine number of clusters and FAT sub-type */
			if(fsty == FS_FAT32) {					/* FAT32 volume */
				if(pau == 0) {						/* AU auto-selection */
					n = (uint32_t)sz_vol / 0x20000; /* Volume size in unit of 128KS */
					for(i = 0, pau = 1; cst32[i] && cst32[i] <= n; i++, pau <<= 1)
						; /* Get from table */
				}
				n_clst = sz_vol / pau;									 /* Number of clusters */
				sz_fat = (n_clst * 4 + 8 + sectorSize - 1) / sectorSize; /* FAT size [sector] */
				sz_rsv = 32;											 /* Number of reserved sectors */
				sz_dir = 0;												 /* No static directory */
				if(n_clst <= MAX_FAT16 || n_clst > MAX_FAT32) {
					return FR_MKFS_ABORTED;
				}
			} else {
				/* FAT volume */
				uint32_t n; // FAT size [byte]
				if(pau == 0) {
					// au auto-selection
					n = sz_vol / 0x1000; // Volume size in unit of 4KS
					// Get from table
					for(unsigned i = 0, pau = 1; cst[i] != 0 && cst[i] <= n; ++i, pau <<= 1) {
					}
				}
				n_clst = sz_vol / pau;
				if(n_clst > MAX_FAT12) {
					n = n_clst * 2 + 4;
				} else {
					fsty = FS_FAT12;
					n = (n_clst * 3 + 1) / 2 + 3;
				}
				sz_fat = (n + sectorSize - 1) / sectorSize;
				sz_rsv = 1;
				sz_dir = n_root * sizeof(msdos_dir_entry) / sectorSize;
			}
			LBA_t b_fat = b_vol + sz_rsv;					// FAT base sector
			LBA_t b_data = b_fat + sz_fat * n_fat + sz_dir; // Data base sector

			/* Align data area to erase block boundary (for flash memory media) */
			uint32_t n = align_up(b_data, sz_blk) - b_data; // Sectors to next nearest from current data base
			if(fsty == FS_FAT32) {							/* FAT32: Move FAT */
				sz_rsv += n;
				b_fat += n;
			} else { /* FAT: Expand FAT */
				if(n % n_fat != 0) {
					// Adjust fractional error
					--n;
					++sz_rsv;
					++b_fat;
				}
				sz_fat += n / n_fat;
			}

			/* Determine number of clusters and final check of validity of the FAT sub-type */
			if(sz_vol < b_data + pau * 16 - b_vol) {
				// Volume too small
				return FR_MKFS_ABORTED;
			}
			n_clst = (sz_vol - sz_rsv - sz_fat * n_fat - sz_dir) / pau;
			if(fsty == FS_FAT32) {
				if(n_clst <= MAX_FAT16) {
					// Too few clusters for FAT32
					if(sz_au == 0) {
						// Adjust cluster size and retry
						sz_au = pau / 2;
						if(sz_au != 0) {
							continue;
						}
					}
					return FR_MKFS_ABORTED;
				}
			}

			if(fsty == FS_FAT16) {
				if(n_clst > MAX_FAT16) {
					// Too many clusters for FAT16
					if(sz_au == 0 && (pau * 2) <= 64) {
						// Adjust cluster size and retry
						sz_au = pau * 2;
						continue;
					}
					if(fsopt & FM_FAT32) {
						// Switch type to FAT32 and retry
						fsty = FS_FAT32;
						continue;
					}
					if(sz_au == 0) {
						// Adjust cluster size and retry
						sz_au = pau * 2;
						if(sz_au <= 128) {
							continue;
						}
					}
					return FR_MKFS_ABORTED;
				}

				if(n_clst <= MAX_FAT12) {
					// Too few clusters for FAT16
					if(sz_au == 0) {
						// Adjust cluster size and retry
						sz_au = pau * 2;
						if(sz_au <= 128) {
							continue;
						}
					}
					return FR_MKFS_ABORTED;
				}
			}

			if(fsty == FS_FAT12 && n_clst > MAX_FAT12) {
				// Too many clusters for FAT12
				return FR_MKFS_ABORTED;
			}

			// Ok, cluster configuration is valid
			break;
		} while(true);

#if FF_USE_TRIM
		lba[0] = b_vol;
		lba[1] = b_vol + sz_vol - 1; /* Inform storage device that the volume area may be erased */
		disk_ioctl(pdrv, CTRL_TRIM, lba);
#endif
		/* Create FAT VBR */
		memset(workBuffer, 0, sectorSize);
		auto& bpb = *static_cast<FAT::fat_boot_sector*>(workBuffer);
		bpb = {
			.jmp_boot = {0xEB, 0xFE, 0x90},
			.system_id = {'M', 'S', 'D', 'O', 'S', '5', '.', '0'},
			.sector_size = sectorSize,
			.sec_per_clus = uint8_t(pau),
			.reserved = uint16_t(sz_rsv),
			.num_fats = uint8_t(n_fat),
			.dir_entries = uint16_t((fsty == FS_FAT32) ? 0 : n_root),
			.sectors = uint16_t((sz_vol < 0x10000) ? sz_vol : 0), // Volume size in 16-bit LBA
			.media = 0xF8,
			.fat_length = uint16_t((fsty != FS_FAT32) ? sz_fat : 0),
			.secs_track = 64,								// Number of sectors per track (for int13)
			.heads = 255,									// Number of heads (for int13)
			.hidden = b_vol,								// Volume offset in the physical drive [sector]
			.total_sect = (sz_vol >= 0x10000) ? sz_vol : 0, // Volume size in 32-bit LBA
		};
		if(fsty == FS_FAT32) {
			bpb.fat32 = {
				.vol_id = vsn,
				.fat_length = sz_fat,
				.root_cluster = 2,
				.info_sector = 1,	 // Offset of FSINFO sector (VBR + 1)
				.backup_boot = 6,	 // Offset of backup VBR (VBR + 6)
				.drive_number = 0x80, // Drive number (for int13)
				.signature = 0x29,	// Extended boot signature
				.vol_label = {'N', 'O', ' ', 'N', 'A', 'M', 'E', ' ', ' ', ' ', ' '},
				.fs_type = FSTYPE_FAT32,
			};
		} else {
			bpb.fat = {
				.vol_id = vsn,
				.drive_number = 0x80, // Drive number (for int13)
				.signature = 0x29,	// Extended boot signature
				.vol_label = {'N', 'O', ' ', 'N', 'A', 'M', 'E', ' ', ' ', ' ', ' '},
				.fs_type = FSTYPE_FAT,
			};
		}
		bpb.signature = BOOT_SIGNATURE;
		if(disk_write(pdrv, workBuffer, b_vol, 1) != RES_OK) {
			return FR_DISK_ERR;
		}

		/* Create FSINFO record if needed */
		if(fsty == FS_FAT32) {
			disk_write(pdrv, workBuffer, b_vol + 6, 1); /* Write backup VBR (VBR + 6) */
			memset(workBuffer, 0, sectorSize);
			auto& fsinfo = *static_cast<fat_boot_fsinfo*>(workBuffer);
			*fsinfo = {
				.signature1 = FAT_FSINFO_SIG1,
				.signature2 = FAT_FSINFO_SIG2,
				.free_clusters = n_clst - 1,
				.next_cluster = 2,
				.signature = BOOT_SIGNATURE,
			};
			disk_write(pdrv, workBuffer, b_vol + 7, 1); // Write backup FSINFO (VBR + 7)
			disk_write(pdrv, workBuffer, b_vol + 1, 1); // Write original FSINFO (VBR + 1)
		}

		/* Initialize FAT area */
		memset(workBuffer, 0, sz_buf * sectorSize);
		sect = b_fat;
		for(i = 0; i < n_fat; ++i) {
			// Initialize each FAT
			if(fsty == FS_FAT32) {
				auto& fat = static_cast<uint32_t*>(workBuffer);
				fat[0] = 0xFFFFFFF8;
				fat[1] = 0xFFFFFFFF;
				fat[2] = 0x0FFFFFFF; // root directory
			} else {
				auto& fat = static_cast<uint16_t*>(workBuffer);
				fat[0] = 0xFFF8;
				fat[1] = (fsty == FS_FAT12) ? 0x00FF : 0xFFFF;
			}
			nsect = sz_fat;
			do {
				// Fill FAT sectors
				n = (nsect > sz_buf) ? sz_buf : nsect;
				if(disk_write(pdrv, buf, sect, n) != RES_OK) {
					return FR_DISK_ERR;
				}
				// Rest of FAT is empty
				memset(workBuffer, 0, sectorSize);
				sect += n;
				nsect -= n;
			} while(nsect);
		}

		/* Initialize root directory (fill with zero) */
		nsect = (fsty == FS_FAT32) ? pau : sz_dir;
		do {
			n = (nsect > sz_buf) ? sz_buf : nsect;
			if(disk_write(pdrv, buf, sect, (unsigned)n) != RES_OK) {
				return FR_DISK_ERR;
			}
			sect += n;
			nsect -= n;
		} while(nsect != 0);
	}

	/* A FAT volume has been created here */

	/* Determine system ID in the MBR partition table */
	if(FF_FS_EXFAT && fsty == FS_EXFAT) {
		// exFAT
		sys = 0x07;
	} else if(fsty == FS_FAT32) {
		// FAT32X
		sys = 0x0C;
	} else if(sz_vol >= 0x10000) {
		// FAT12/16 (large)
		sys = 0x06;
	} else if(fsty == FS_FAT16) {
		sys = 0x04;
	} else {
		sys = 0x01;
	}

	/* Update partition information */
	if(FF_MULTI_PARTITION && ipart != 0) { /* Volume is in the existing partition */
		if(!FF_LBA64 || !(fsopt & 0x80)) {
			/* Update system ID in the partition table (MBR) */
			if(disk_read(pdrv, workBuffer, 0, 1) != RES_OK) {
				return FR_DISK_ERR;
			}
			auto& mbr = *reinterpret_cast<legacy_mbr*>(workBuffer);
			mbr->partition_record[ipart - 1].os_type = sys;
			if(disk_write(pdrv, buf, 0, 1) != RES_OK) {
				return (FR_DISK_ERR);
			}
		}
	} else if(!(fsopt & FM_SFD)) {
		// Not in SFD: create partition table
		lba[0] = sz_vol;
		lba[1] = 0;
		fr = create_partition(pdrv, lba, sys, buf);
		if(fr != FR_OK) {
			return fr;
		}
	}

	if(disk_ioctl(pdrv, CTRL_SYNC, 0) != RES_OK) {
		return FR_DISK_ERR;
	}

	return FR_OK;
}

} // namespace Storage
