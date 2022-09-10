#include "include/Storage/DiskDevice.h"
#include <Storage/CustomDevice.h>
#include <debug_progmem.h>

extern "C" int os_get_random(void* buf, size_t len);

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
		unsigned num_partition_entries = gpt->num_partition_entries;
		uint64_t gptEntryOffset = gpt->partition_entry_lba * SECTOR_SIZE;
		for(unsigned i = 0; i < num_partition_entries; i++) {
			if(gptEntryOffset % SECTOR_SIZE == 0) {
				if(!device.read(gptEntryOffset, buffer, SECTOR_SIZE)) {
					break;
				}
			}

			auto entry = reinterpret_cast<gpt_entry*>(&buffer[gptEntryOffset % SECTOR_SIZE]);
			if(entry->partition_type_guid == PARTITION_BASIC_DATA_GUID) {
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

/*-----------------------------------------------------------------------*/
/* Create FAT/exFAT volume (with sub-functions)                          */
/*-----------------------------------------------------------------------*/

#define N_SEC_TRACK 63	 /* Sectors per track for determination of drive CHS */
#define GPT_ALIGN 0x100000 /* Alignment of partitions in GPT [byte] (>=128KB) */
#define GPT_ITEMS 128	  /* Number of GPT table size (>=128, sector aligned) */

/* Create partitions on the physical drive in format of MBR or GPT */
static FRESULT create_partition(uint8_t drv,		/* Physical drive number */
								const LBA_t plst[], /* Partition list */
								uint8_t sys,		/* System ID (for only MBR, temp setting) */
								uint8_t* buf		/* Working buffer for a sector */
)
{
	unsigned cy;
	LBA_t sz_drv;
	uint32_t sz_part32;
	uint8_t hd, sc;

	/* Get physical drive size */
	if(disk_ioctl(drv, GET_SECTOR_COUNT, &sz_drv) != RES_OK)
		return FR_DISK_ERR;

#if FF_LBA64
	if(sz_drv >= FF_MIN_GPT) { /* Create partitions in GPT format */
		uint16_t ss;
		unsigned ofs;

#if FF_MAX_SS != FF_MIN_SS
		if(disk_ioctl(drv, GET_SECTOR_SIZE, &ss) != RES_OK) {
			return FR_DISK_ERR; /* Get sector size */
		}
		if(ss > FF_MAX_SS || ss < FF_MIN_SS || (ss & (ss - 1))) {
			return FR_DISK_ERR;
		}
#else
		ss = FF_MAX_SS;
#endif
		uint32_t rnd = (uint32_t)sz_drv + GET_FATTIME(); /* Random seed */
		uint32_t align = GPT_ALIGN / ss;				 /* Partition alignment for GPT [sector] */
		unsigned sz_ptbl = GPT_ITEMS * SZ_GPTE / ss;	 /* Size of partition table [sector] */
		uint64_t top_bpt = sz_drv - sz_ptbl - 1;		 /* Backup partiiton table start sector */
		uint64_t nxt_alloc = 2 + sz_ptbl;				 /* First allocatable sector */
		uint64_t sz_pool = top_bpt - nxt_alloc;			 /* Size of allocatable area */
		uint32_t bcc = 0xffffffff;
		uint64_t sz_part = 1;
		unsigned pi = 0; // partition table index
		unsigned si = 0; // size table index */
		do {
			if(pi * SZ_GPTE % ss == 0) {
				memset(buf, 0, ss); /* Clean the buffer if needed */
			}
			// Is the size table not termintated?
			if(sz_part != 0) {
				// Align partition start
				nxt_alloc = (nxt_alloc + align - 1) & ((uint64_t)0 - align);
				sz_part = plst[si++]; // Get a partition size
				// Is the size in percentage?
				if(sz_part <= 100) {
					sz_part = sz_pool * sz_part / 100;
					// Align partition end
					sz_part = (sz_part + align - 1) & (0ULL - align);
				}
				// Clip the size at end of the pool
				if(nxt_alloc + sz_part > top_bpt) {
					sz_part = (nxt_alloc < top_bpt) ? top_bpt - nxt_alloc : 0;
				}
			}

			// Add a partition?
			if(sz_part != 0) {
				ofs = pi * SZ_GPTE % ss;
				auto entry = reinterpret_cast<gpt_entry*>(&buf[ofs]);
				entry->partition_type_guid = PARTITION_BASIC_DATA_GUID;
				os_get_random(&entry->unique_partition_guid, sizeof(efi_guid_t));
				entry->starting_lba = nxt_alloc;
				entry->ending_lba = nxt_alloc + sz_part - 1;
				nxt_alloc += sz_part; // Next allocatable sector
			}

			// Write the buffer if it is filled up
			if((pi + 1) * SZ_GPTE % ss == 0) {
				bcc = crc32(buf, ss);
				// Write to primary table
				if(disk_write(drv, buf, 2 + pi * SZ_GPTE / ss, 1) != RES_OK) {
					return FR_DISK_ERR;
				}
				// Write to secondary table
				if(disk_write(drv, buf, top_bpt + pi * SZ_GPTE / ss, 1) != RES_OK) {
					return FR_DISK_ERR;
				}
			}
		} while(++pi < GPT_ITEMS);

		/* Create primary GPT header */
		auto& header = *reinterpret_cast<gpt_header*>(buf);
		header = {
			.signature = GPT_HEADER_SIGNATURE,
			.revision = GPT_HEADER_REVISION_V1,
			.header_size = sizeof(gpt_header),
			.my_lba = 1,
			.alternate_lba == sz_drv - 1,
			.first_usable_lba = 2 + sz_ptbl,
			.last_usable_lba = top_bpt - 1,
			.partition_entry_lba = 2,
			.num_partition_entries = GPT_ITEMS,
			.sizeof_partition_entry = sizeof(gpt_entry),
			.partition_entry_array_crc32 = bcc,
		};
		os_get_random(&header.disk_guid, sizeof(efi_guid_t));
		header.header_crc32 = crc32(&header, sizeof(header));

		if(disk_write(drv, buf, 1, 1) != RES_OK) {
			return FR_DISK_ERR;
		}

		/* Create secondary GPT header */
		std::swap(header.my_lba, header.alternate_lba);
		header.partition_entry_lba = top_bpt;
		header.header_crc32 = 0;
		header.header_crc32 = crc32(&header, sizeof(header));

		if(disk_write(drv, buf, sz_drv - 1, 1) != RES_OK) {
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
		memcpy_P(&mbr.partition_record[0], gpt_mbr, sizeof(gpt_mbr));
		if(disk_write(drv, buf, 0, 1) != RES_OK) {
			return FR_DISK_ERR;
		}
	} else
#endif
	{ /* Create partitions in MBR format */
		uint32_t sz_drv32 = sz_drv;
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
		for(i = 0, nxt_alloc32 = n_sc; i < 4 && nxt_alloc32 != 0 && nxt_alloc32 < sz_drv32;
			i++, nxt_alloc32 += sz_part32) {
			uint32_t sz_part32 = plst[i]; /* Get partition size */
			if(sz_part32 <= 100) {
				sz_part32 = (sz_part32 == 100) ? sz_drv32 : sz_drv32 / 100 * sz_part32; /* Size in percentage? */
			}
			if(nxt_alloc32 + sz_part32 > sz_drv32 || nxt_alloc32 + sz_part32 < nxt_alloc32) {
				sz_part32 = sz_drv32 - nxt_alloc32; /* Clip at drive size */
			}
			if(sz_part32 == 0) {
				break; /* End of table or no sector to allocate? */
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
		}

		st_word(buf + BS_55AA, 0xAA55); /* MBR signature */
		if(disk_write(drv, buf, 0, 1) != RES_OK)
			return FR_DISK_ERR; /* Write it to the MBR */
	}

	return FR_OK;
}

FRESULT f_mkfs(const TCHAR* path,	/* Logical drive number */
			   const MKFS_PARM* opt, /* Format options */
			   void* work,			 /* Pointer to working buffer (null: use heap memory) */
			   unsigned len			 /* Size of working buffer [byte] */
)
{
	static const uint16_t cst[] = {1, 4, 16, 64, 256, 512, 0}; /* Cluster size boundary for FAT volume (4Ks unit) */
	static const uint16_t cst32[] = {1, 2, 4, 8, 16, 32, 0};   /* Cluster size boundary for FAT32 volume (128Ks unit) */
	static const MKFS_PARM defopt = {FM_ANY, 0, 0, 0, 0};	  /* Default parameter */
	uint8_t fsopt, fsty, sys, *buf, *pte, pdrv, ipart;
	uint16_t ss; /* Sector size */
	uint32_t sz_buf, sz_blk, n_clst, pau, nsect, n, vsn;
	LBA_t sz_vol, b_vol, b_fat, b_data; /* Size of volume, Base LBA of volume, fat, data */
	LBA_t sect, lba[2];
	uint32_t sz_rsv, sz_fat, sz_dir, sz_au; /* Size of reserved, fat, dir, data, cluster */
	unsigned n_fat, n_root, i;				/* Index, Number of FATs and Number of roor dir entries */
	FRESULT fr;

	/* Check mounted drive and clear work area */
	currentVolume->getFatFS()->fs_type = 0; /* Clear the fs object if mounted */
	pdrv = 0;								/* Physical drive */
	ipart = LD2PT(vol);						/* Partition (0:create as new, 1..:get from partition table) */
	if(!opt)
		opt = &defopt; /* Use default parameter if it is not given */

	/* Get physical drive status (sz_drv, sz_blk, ss) */
	sz_blk = opt->align;
	if(sz_blk == 0 && disk_ioctl(pdrv, GET_BLOCK_SIZE, &sz_blk) != RES_OK)
		sz_blk = 1;
	if(sz_blk == 0 || sz_blk > 0x8000 || (sz_blk & (sz_blk - 1)))
		sz_blk = 1;
#if FF_MAX_SS != FF_MIN_SS
	if(disk_ioctl(pdrv, GET_SECTOR_SIZE, &ss) != RES_OK)
		return FR_DISK_ERR;
	if(ss > FF_MAX_SS || ss < FF_MIN_SS || (ss & (ss - 1)))
		return FR_DISK_ERR;
#else
	ss = FF_MAX_SS;
#endif
	/* Options for FAT sub-type and FAT parameters */
	fsopt = opt->fmt & (FM_ANY | FM_SFD);
	n_fat = (opt->n_fat >= 1 && opt->n_fat <= 2) ? opt->n_fat : 1;
	n_root = (opt->n_root >= 1 && opt->n_root <= 32768 && (opt->n_root % (ss / SZDIRE)) == 0) ? opt->n_root : 512;
	sz_au = (opt->au_size <= 0x1000000 && (opt->au_size & (opt->au_size - 1)) == 0) ? opt->au_size : 0;
	sz_au /= ss; /* Byte --> Sector */

	/* Get working buffer */
	sz_buf = len / ss; /* Size of working buffer [sector] */
	if(sz_buf == 0)
		return FR_NOT_ENOUGH_CORE;
	buf = (uint8_t*)work; /* Working buffer */
#if FF_USE_LFN == 3
	if(!buf)
		buf = ff_memalloc(sz_buf * ss); /* Use heap memory for working buffer */
#endif
	if(!buf)
		return FR_NOT_ENOUGH_CORE;

	/* Determine where the volume to be located (b_vol, sz_vol) */
	b_vol = sz_vol = 0;
	if(FF_MULTI_PARTITION && ipart != 0) { /* Is the volume associated with any specific partition? */
		/* Get partition location from the existing partition table */
		if(disk_read(pdrv, buf, 0, 1) != RES_OK)
			LEAVE_MKFS(FR_DISK_ERR); /* Load MBR */
		if(ld_word(buf + BS_55AA) != 0xAA55)
			LEAVE_MKFS(FR_MKFS_ABORTED); /* Check if MBR is valid */
#if FF_LBA64
		if(buf[MBR_Table + PTE_System] == 0xEE) { /* GPT protective MBR? */
			uint32_t n_ent, ofs;
			uint64_t pt_lba;

			/* Get the partition location from GPT */
			if(disk_read(pdrv, buf, 1, 1) != RES_OK)
				LEAVE_MKFS(FR_DISK_ERR); /* Load GPT header sector (next to MBR) */
			if(!test_gpt_header(buf))
				LEAVE_MKFS(FR_MKFS_ABORTED);	 /* Check if GPT header is valid */
			n_ent = ld_dword(buf + GPTH_PtNum);  /* Number of entries */
			pt_lba = ld_qword(buf + GPTH_PtOfs); /* Table start sector */
			ofs = i = 0;
			while(n_ent) { /* Find MS Basic partition with order of ipart */
				if(ofs == 0 && disk_read(pdrv, buf, pt_lba++, 1) != RES_OK)
					LEAVE_MKFS(FR_DISK_ERR);											  /* Get PT sector */
				if(!memcmp(buf + ofs + GPTE_PtGuid, GUID_MS_Basic, 16) && ++i == ipart) { /* MS basic data partition? */
					b_vol = ld_qword(buf + ofs + GPTE_FstLba);
					sz_vol = ld_qword(buf + ofs + GPTE_LstLba) - b_vol + 1;
					break;
				}
				n_ent--;
				ofs = (ofs + SZ_GPTE) % ss; /* Next entry */
			}
			if(n_ent == 0)
				LEAVE_MKFS(FR_MKFS_ABORTED); /* Partition not found */
			fsopt |= 0x80;					 /* Partitioning is in GPT */
		} else
#endif
		{ /* Get the partition location from MBR partition table */
			pte = buf + (MBR_Table + (ipart - 1) * SZ_PTE);
			if(ipart > 4 || pte[PTE_System] == 0)
				LEAVE_MKFS(FR_MKFS_ABORTED);	 /* No partition? */
			b_vol = ld_dword(pte + PTE_StLba);   /* Get volume start sector */
			sz_vol = ld_dword(pte + PTE_SizLba); /* Get volume size */
		}
	} else { /* The volume is associated with a physical drive */
		if(disk_ioctl(pdrv, GET_SECTOR_COUNT, &sz_vol) != RES_OK)
			LEAVE_MKFS(FR_DISK_ERR);
		if(!(fsopt & FM_SFD)) { /* To be partitioned? */
								/* Create a single-partition on the drive in this function */
#if FF_LBA64
			if(sz_vol >= FF_MIN_GPT) { /* Which partition type to create, MBR or GPT? */
				fsopt |= 0x80;		   /* Partitioning is in GPT */
				b_vol = GPT_ALIGN / ss;
				sz_vol -= b_vol + GPT_ITEMS * SZ_GPTE / ss + 1; /* Estimated partition offset and size */
			} else
#endif
			{ /* Partitioning is in MBR */
				if(sz_vol > N_SEC_TRACK) {
					b_vol = N_SEC_TRACK;
					sz_vol -= b_vol; /* Estimated partition offset and size */
				}
			}
		}
	}
	if(sz_vol < 128)
		LEAVE_MKFS(FR_MKFS_ABORTED); /* Check if volume size is >=128s */

	/* Now start to create an FAT volume at b_vol and sz_vol */

	do {										/* Pre-determine the FAT type */
		if(FF_FS_EXFAT && (fsopt & FM_EXFAT)) { /* exFAT possible? */
			if((fsopt & FM_ANY) == FM_EXFAT || sz_vol >= 0x4000000 ||
			   sz_au > 128) { /* exFAT only, vol >= 64MS or sz_au > 128S ? */
				fsty = FS_EXFAT;
				break;
			}
		}
#if FF_LBA64
		if(sz_vol >= 0x100000000)
			LEAVE_MKFS(FR_MKFS_ABORTED); /* Too large volume for FAT/FAT32 */
#endif
		if(sz_au > 128)
			sz_au = 128;			/* Invalid AU for FAT/FAT32? */
		if(fsopt & FM_FAT32) {		/* FAT32 possible? */
			if(!(fsopt & FM_FAT)) { /* no-FAT? */
				fsty = FS_FAT32;
				break;
			}
		}
		if(!(fsopt & FM_FAT))
			LEAVE_MKFS(FR_INVALID_PARAMETER); /* no-FAT? */
		fsty = FS_FAT16;
	} while(0);

	vsn = (uint32_t)sz_vol + GET_FATTIME(); /* VSN generated from current time and partitiion size */

#if FF_FS_EXFAT
	if(fsty == FS_EXFAT) { /* Create an exFAT volume */
		uint32_t szb_bit, szb_case, sum, nbit, clu, clen[3];
		WCHAR ch, si;
		unsigned j, st;

		if(sz_vol < 0x1000)
			LEAVE_MKFS(FR_MKFS_ABORTED); /* Too small volume for exFAT? */
#if FF_USE_TRIM
		lba[0] = b_vol;
		lba[1] = b_vol + sz_vol - 1; /* Inform storage device that the volume area may be erased */
		disk_ioctl(pdrv, CTRL_TRIM, lba);
#endif
		/* Determine FAT location, data location and number of clusters */
		if(sz_au == 0) { /* AU auto-selection */
			if(sz_vol >= 0x4000000) {
				sz_au = 256; // >= 64Ms
			} else if(sz_vol >= 0x80000) {
				sz_au = 64; // >= 512Ks
			} else {
				sz_au = 8;
			}
		}
		b_fat = b_vol + 32;											   /* FAT start at offset 32 */
		sz_fat = (uint32_t)((sz_vol / sz_au + 2) * 4 + ss - 1) / ss;   /* Number of FAT sectors */
		b_data = (b_fat + sz_fat + sz_blk - 1) & ~((LBA_t)sz_blk - 1); /* Align data area to the erase block boundary */
		if(b_data - b_vol >= sz_vol / 2)
			LEAVE_MKFS(FR_MKFS_ABORTED);						/* Too small volume? */
		n_clst = (uint32_t)(sz_vol - (b_data - b_vol)) / sz_au; /* Number of clusters */
		if(n_clst < 16)
			LEAVE_MKFS(FR_MKFS_ABORTED); /* Too few clusters? */
		if(n_clst > MAX_EXFAT)
			LEAVE_MKFS(FR_MKFS_ABORTED); /* Too many clusters? */

		szb_bit = (n_clst + 7) / 8;							 /* Size of allocation bitmap */
		clen[0] = (szb_bit + sz_au * ss - 1) / (sz_au * ss); /* Number of allocation bitmap clusters */

		/* Create a compressed up-case table */
		sect = b_data + sz_au * clen[0]; /* Table start sector */
		sum = 0;						 /* Table checksum to be stored in the 82 entry */
		st = 0;
		si = 0;
		i = 0;
		j = 0;
		szb_case = 0;
		do {
			switch(st) {
			case 0:
				ch = (WCHAR)ff_wtoupper(si); /* Get an up-case char */
				if(ch != si) {
					si++;
					break; /* Store the up-case char if exist */
				}
				for(j = 1; (WCHAR)(si + j) && (WCHAR)(si + j) == ff_wtoupper((WCHAR)(si + j)); j++)
					; /* Get run length of no-case block */
				if(j >= 128) {
					ch = 0xFFFF;
					st = 2;
					break; /* Compress the no-case block if run is >= 128 chars */
				}
				st = 1; /* Do not compress short run */
						/* FALLTHROUGH */
			case 1:
				ch = si++; /* Fill the short run */
				if(--j == 0)
					st = 0;
				break;

			default:
				ch = (WCHAR)j;
				si += (WCHAR)j; /* Number of chars to skip */
				st = 0;
			}
			sum = xsum32(buf[i + 0] = (uint8_t)ch, sum); /* Put it into the write buffer */
			sum = xsum32(buf[i + 1] = (uint8_t)(ch >> 8), sum);
			i += 2;
			szb_case += 2;
			if(si == 0 || i == sz_buf * ss) { /* Write buffered data when buffer full or end of process */
				n = (i + ss - 1) / ss;
				if(disk_write(pdrv, buf, sect, n) != RES_OK)
					LEAVE_MKFS(FR_DISK_ERR);
				sect += n;
				i = 0;
			}
		} while(si);
		clen[1] = (szb_case + sz_au * ss - 1) / (sz_au * ss); /* Number of up-case table clusters */
		clen[2] = 1;										  /* Number of root dir clusters */

		/* Initialize the allocation bitmap */
		sect = b_data;
		nsect = (szb_bit + ss - 1) / ss;	/* Start of bitmap and number of bitmap sectors */
		nbit = clen[0] + clen[1] + clen[2]; /* Number of clusters in-use by system (bitmap, up-case and root-dir) */
		do {
			memset(buf, 0, sz_buf * ss); /* Initialize bitmap buffer */
			for(i = 0; nbit != 0 && i / 8 < sz_buf * ss; buf[i / 8] |= 1 << (i % 8), i++, nbit--)
				;								   /* Mark used clusters */
			n = (nsect > sz_buf) ? sz_buf : nsect; /* Write the buffered data */
			if(disk_write(pdrv, buf, sect, n) != RES_OK)
				LEAVE_MKFS(FR_DISK_ERR);
			sect += n;
			nsect -= n;
		} while(nsect);

		/* Initialize the FAT */
		sect = b_fat;
		nsect = sz_fat; /* Start of FAT and number of FAT sectors */
		j = nbit = clu = 0;
		do {
			memset(buf, 0, sz_buf * ss);
			i = 0;		   /* Clear work area and reset write offset */
			if(clu == 0) { /* Initialize FAT [0] and FAT[1] */
				st_dword(buf + i, 0xFFFFFFF8);
				i += 4;
				clu++;
				st_dword(buf + i, 0xFFFFFFFF);
				i += 4;
				clu++;
			}
			do {									  /* Create chains of bitmap, up-case and root dir */
				while(nbit != 0 && i < sz_buf * ss) { /* Create a chain */
					st_dword(buf + i, (nbit > 1) ? clu + 1 : 0xFFFFFFFF);
					i += 4;
					clu++;
					nbit--;
				}
				if(nbit == 0 && j < 3)
					nbit = clen[j++]; /* Get next chain length */
			} while(nbit != 0 && i < sz_buf * ss);
			n = (nsect > sz_buf) ? sz_buf : nsect; /* Write the buffered data */
			if(disk_write(pdrv, buf, sect, n) != RES_OK)
				LEAVE_MKFS(FR_DISK_ERR);
			sect += n;
			nsect -= n;
		} while(nsect);

		/* Initialize the root directory */
		memset(buf, 0, sz_buf * ss);
		buf[SZDIRE * 0 + 0] = ET_VLABEL;			  /* Volume label entry (no label) */
		buf[SZDIRE * 1 + 0] = ET_BITMAP;			  /* Bitmap entry */
		st_dword(buf + SZDIRE * 1 + 20, 2);			  /*  cluster */
		st_dword(buf + SZDIRE * 1 + 24, szb_bit);	 /*  size */
		buf[SZDIRE * 2 + 0] = ET_UPCASE;			  /* Up-case table entry */
		st_dword(buf + SZDIRE * 2 + 4, sum);		  /*  sum */
		st_dword(buf + SZDIRE * 2 + 20, 2 + clen[0]); /*  cluster */
		st_dword(buf + SZDIRE * 2 + 24, szb_case);	/*  size */
		sect = b_data + sz_au * (clen[0] + clen[1]);
		nsect = sz_au; /* Start of the root directory and number of sectors */
		do {		   /* Fill root directory sectors */
			n = (nsect > sz_buf) ? sz_buf : nsect;
			if(disk_write(pdrv, buf, sect, n) != RES_OK)
				LEAVE_MKFS(FR_DISK_ERR);
			memset(buf, 0, ss); /* Rest of entries are filled with zero */
			sect += n;
			nsect -= n;
		} while(nsect);

		/* Create two set of the exFAT VBR blocks */
		sect = b_vol;
		for(n = 0; n < 2; n++) {
			/* Main record (+0) */
			memset(buf, 0, ss);
			memcpy(buf + BS_JmpBoot,
				   "\xEB\x76\x90"
				   "EXFAT   ",
				   11);												 /* Boot jump code (x86), OEM name */
			st_qword(buf + BPB_VolOfsEx, b_vol);					 /* Volume offset in the physical drive [sector] */
			st_qword(buf + BPB_TotSecEx, sz_vol);					 /* Volume size [sector] */
			st_dword(buf + BPB_FatOfsEx, (uint32_t)(b_fat - b_vol)); /* FAT offset [sector] */
			st_dword(buf + BPB_FatSzEx, sz_fat);					 /* FAT size [sector] */
			st_dword(buf + BPB_DataOfsEx, (uint32_t)(b_data - b_vol)); /* Data offset [sector] */
			st_dword(buf + BPB_NumClusEx, n_clst);					   /* Number of clusters */
			st_dword(buf + BPB_RootClusEx, 2 + clen[0] + clen[1]);	 /* Root dir cluster # */
			st_dword(buf + BPB_VolIDEx, vsn);						   /* VSN */
			st_word(buf + BPB_FSVerEx, 0x100);						   /* Filesystem version (1.00) */
			for(buf[BPB_BytsPerSecEx] = 0, i = ss; i >>= 1; buf[BPB_BytsPerSecEx]++)
				; /* Log2 of sector size [byte] */
			for(buf[BPB_SecPerClusEx] = 0, i = sz_au; i >>= 1; buf[BPB_SecPerClusEx]++)
				;								  /* Log2 of cluster size [sector] */
			buf[BPB_NumFATsEx] = 1;				  /* Number of FATs */
			buf[BPB_DrvNumEx] = 0x80;			  /* Drive number (for int13) */
			st_word(buf + BS_BootCodeEx, 0xFEEB); /* Boot code (x86) */
			st_word(buf + BS_55AA, 0xAA55);		  /* Signature (placed here regardless of sector size) */
			for(i = sum = 0; i < ss; i++) {		  /* VBR checksum */
				if(i != BPB_VolFlagEx && i != BPB_VolFlagEx + 1 && i != BPB_PercInUseEx)
					sum = xsum32(buf[i], sum);
			}
			if(disk_write(pdrv, buf, sect++, 1) != RES_OK)
				LEAVE_MKFS(FR_DISK_ERR);
			/* Extended bootstrap record (+1..+8) */
			memset(buf, 0, ss);
			st_word(buf + ss - 2, 0xAA55); /* Signature (placed at end of sector) */
			for(j = 1; j < 9; j++) {
				for(i = 0; i < ss; sum = xsum32(buf[i++], sum))
					; /* VBR checksum */
				if(disk_write(pdrv, buf, sect++, 1) != RES_OK)
					LEAVE_MKFS(FR_DISK_ERR);
			}
			/* OEM/Reserved record (+9..+10) */
			memset(buf, 0, ss);
			for(; j < 11; j++) {
				for(i = 0; i < ss; sum = xsum32(buf[i++], sum))
					; /* VBR checksum */
				if(disk_write(pdrv, buf, sect++, 1) != RES_OK)
					LEAVE_MKFS(FR_DISK_ERR);
			}
			/* Sum record (+11) */
			for(i = 0; i < ss; i += 4)
				st_dword(buf + i, sum); /* Fill with checksum value */
			if(disk_write(pdrv, buf, sect++, 1) != RES_OK)
				LEAVE_MKFS(FR_DISK_ERR);
		}

	} else
#endif /* FF_FS_EXFAT */
	{  /* Create an FAT/FAT32 volume */
		do {
			pau = sz_au;
			/* Pre-determine number of clusters and FAT sub-type */
			if(fsty == FS_FAT32) {					/* FAT32 volume */
				if(pau == 0) {						/* AU auto-selection */
					n = (uint32_t)sz_vol / 0x20000; /* Volume size in unit of 128KS */
					for(i = 0, pau = 1; cst32[i] && cst32[i] <= n; i++, pau <<= 1)
						; /* Get from table */
				}
				n_clst = (uint32_t)sz_vol / pau;		 /* Number of clusters */
				sz_fat = (n_clst * 4 + 8 + ss - 1) / ss; /* FAT size [sector] */
				sz_rsv = 32;							 /* Number of reserved sectors */
				sz_dir = 0;								 /* No static directory */
				if(n_clst <= MAX_FAT16 || n_clst > MAX_FAT32)
					LEAVE_MKFS(FR_MKFS_ABORTED);
			} else {							   /* FAT volume */
				if(pau == 0) {					   /* au auto-selection */
					n = (uint32_t)sz_vol / 0x1000; /* Volume size in unit of 4KS */
					for(i = 0, pau = 1; cst[i] && cst[i] <= n; i++, pau <<= 1)
						; /* Get from table */
				}
				n_clst = (uint32_t)sz_vol / pau;
				if(n_clst > MAX_FAT12) {
					n = n_clst * 2 + 4; /* FAT size [byte] */
				} else {
					fsty = FS_FAT12;
					n = (n_clst * 3 + 1) / 2 + 3; /* FAT size [byte] */
				}
				sz_fat = (n + ss - 1) / ss;				 /* FAT size [sector] */
				sz_rsv = 1;								 /* Number of reserved sectors */
				sz_dir = (uint32_t)n_root * SZDIRE / ss; /* Root dir size [sector] */
			}
			b_fat = b_vol + sz_rsv;					  /* FAT base */
			b_data = b_fat + sz_fat * n_fat + sz_dir; /* Data base */

			/* Align data area to erase block boundary (for flash memory media) */
			n = (uint32_t)(((b_data + sz_blk - 1) & ~(sz_blk - 1)) -
						   b_data); /* Sectors to next nearest from current data base */
			if(fsty == FS_FAT32) {  /* FAT32: Move FAT */
				sz_rsv += n;
				b_fat += n;
			} else {			/* FAT: Expand FAT */
				if(n % n_fat) { /* Adjust fractional error if needed */
					n--;
					sz_rsv++;
					b_fat++;
				}
				sz_fat += n / n_fat;
			}

			/* Determine number of clusters and final check of validity of the FAT sub-type */
			if(sz_vol < b_data + pau * 16 - b_vol)
				LEAVE_MKFS(FR_MKFS_ABORTED); /* Too small volume? */
			n_clst = ((uint32_t)sz_vol - sz_rsv - sz_fat * n_fat - sz_dir) / pau;
			if(fsty == FS_FAT32) {
				if(n_clst <= MAX_FAT16) { /* Too few clusters for FAT32? */
					if(sz_au == 0 && (sz_au = pau / 2) != 0)
						continue; /* Adjust cluster size and retry */
					LEAVE_MKFS(FR_MKFS_ABORTED);
				}
			}
			if(fsty == FS_FAT16) {
				if(n_clst > MAX_FAT16) { /* Too many clusters for FAT16 */
					if(sz_au == 0 && (pau * 2) <= 64) {
						sz_au = pau * 2;
						continue; /* Adjust cluster size and retry */
					}
					if((fsopt & FM_FAT32)) {
						fsty = FS_FAT32;
						continue; /* Switch type to FAT32 and retry */
					}
					if(sz_au == 0 && (sz_au = pau * 2) <= 128)
						continue; /* Adjust cluster size and retry */
					LEAVE_MKFS(FR_MKFS_ABORTED);
				}
				if(n_clst <= MAX_FAT12) { /* Too few clusters for FAT16 */
					if(sz_au == 0 && (sz_au = pau * 2) <= 128)
						continue; /* Adjust cluster size and retry */
					LEAVE_MKFS(FR_MKFS_ABORTED);
				}
			}
			if(fsty == FS_FAT12 && n_clst > MAX_FAT12)
				LEAVE_MKFS(FR_MKFS_ABORTED); /* Too many clusters for FAT12 */

			/* Ok, it is the valid cluster configuration */
			break;
		} while(1);

#if FF_USE_TRIM
		lba[0] = b_vol;
		lba[1] = b_vol + sz_vol - 1; /* Inform storage device that the volume area may be erased */
		disk_ioctl(pdrv, CTRL_TRIM, lba);
#endif
		/* Create FAT VBR */
		memset(buf, 0, ss);
		memcpy(buf + BS_JmpBoot,
			   "\xEB\xFE\x90"
			   "MSDOS5.0",
			   11);										 /* Boot jump code (x86), OEM name */
		st_word(buf + BPB_BytsPerSec, ss);				 /* Sector size [byte] */
		buf[BPB_SecPerClus] = (uint8_t)pau;				 /* Cluster size [sector] */
		st_word(buf + BPB_RsvdSecCnt, (uint16_t)sz_rsv); /* Size of reserved area */
		buf[BPB_NumFATs] = (uint8_t)n_fat;				 /* Number of FATs */
		st_word(buf + BPB_RootEntCnt,
				(uint16_t)((fsty == FS_FAT32) ? 0 : n_root)); /* Number of root directory entries */
		if(sz_vol < 0x10000) {
			st_word(buf + BPB_TotSec16, (uint16_t)sz_vol); /* Volume size in 16-bit LBA */
		} else {
			st_dword(buf + BPB_TotSec32, (uint32_t)sz_vol); /* Volume size in 32-bit LBA */
		}
		buf[BPB_Media] = 0xF8;						  /* Media descriptor byte */
		st_word(buf + BPB_SecPerTrk, 63);			  /* Number of sectors per track (for int13) */
		st_word(buf + BPB_NumHeads, 255);			  /* Number of heads (for int13) */
		st_dword(buf + BPB_HiddSec, (uint32_t)b_vol); /* Volume offset in the physical drive [sector] */
		if(fsty == FS_FAT32) {
			st_dword(buf + BS_VolID32, vsn);	 /* VSN */
			st_dword(buf + BPB_FATSz32, sz_fat); /* FAT size [sector] */
			st_dword(buf + BPB_RootClus32, 2);   /* Root directory cluster # (2) */
			st_word(buf + BPB_FSInfo32, 1);		 /* Offset of FSINFO sector (VBR + 1) */
			st_word(buf + BPB_BkBootSec32, 6);   /* Offset of backup VBR (VBR + 6) */
			buf[BS_DrvNum32] = 0x80;			 /* Drive number (for int13) */
			buf[BS_BootSig32] = 0x29;			 /* Extended boot signature */
			memcpy(buf + BS_VolLab32,
				   "NO NAME    "
				   "FAT32   ",
				   19); /* Volume label, FAT signature */
		} else {
			st_dword(buf + BS_VolID, vsn);				  /* VSN */
			st_word(buf + BPB_FATSz16, (uint16_t)sz_fat); /* FAT size [sector] */
			buf[BS_DrvNum] = 0x80;						  /* Drive number (for int13) */
			buf[BS_BootSig] = 0x29;						  /* Extended boot signature */
			memcpy(buf + BS_VolLab,
				   "NO NAME    "
				   "FAT     ",
				   19); /* Volume label, FAT signature */
		}
		st_word(buf + BS_55AA, 0xAA55); /* Signature (offset is fixed here regardless of sector size) */
		if(disk_write(pdrv, buf, b_vol, 1) != RES_OK)
			LEAVE_MKFS(FR_DISK_ERR); /* Write it to the VBR sector */

		/* Create FSINFO record if needed */
		if(fsty == FS_FAT32) {
			disk_write(pdrv, buf, b_vol + 6, 1); /* Write backup VBR (VBR + 6) */
			memset(buf, 0, ss);
			st_dword(buf + FSI_LeadSig, 0x41615252);
			st_dword(buf + FSI_StrucSig, 0x61417272);
			st_dword(buf + FSI_Free_Count, n_clst - 1); /* Number of free clusters */
			st_dword(buf + FSI_Nxt_Free, 2);			/* Last allocated cluster# */
			st_word(buf + BS_55AA, 0xAA55);
			disk_write(pdrv, buf, b_vol + 7, 1); /* Write backup FSINFO (VBR + 7) */
			disk_write(pdrv, buf, b_vol + 1, 1); /* Write original FSINFO (VBR + 1) */
		}

		/* Initialize FAT area */
		memset(buf, 0, sz_buf * ss);
		sect = b_fat;				 /* FAT start sector */
		for(i = 0; i < n_fat; i++) { /* Initialize FATs each */
			if(fsty == FS_FAT32) {
				st_dword(buf + 0, 0xFFFFFFF8); /* FAT[0] */
				st_dword(buf + 4, 0xFFFFFFFF); /* FAT[1] */
				st_dword(buf + 8, 0x0FFFFFFF); /* FAT[2] (root directory) */
			} else {
				st_dword(buf + 0, (fsty == FS_FAT12) ? 0xFFFFF8 : 0xFFFFFFF8); /* FAT[0] and FAT[1] */
			}
			nsect = sz_fat; /* Number of FAT sectors */
			do {			/* Fill FAT sectors */
				n = (nsect > sz_buf) ? sz_buf : nsect;
				if(disk_write(pdrv, buf, sect, (unsigned)n) != RES_OK)
					LEAVE_MKFS(FR_DISK_ERR);
				memset(buf, 0, ss); /* Rest of FAT all are cleared */
				sect += n;
				nsect -= n;
			} while(nsect);
		}

		/* Initialize root directory (fill with zero) */
		nsect = (fsty == FS_FAT32) ? pau : sz_dir; /* Number of root directory sectors */
		do {
			n = (nsect > sz_buf) ? sz_buf : nsect;
			if(disk_write(pdrv, buf, sect, (unsigned)n) != RES_OK)
				LEAVE_MKFS(FR_DISK_ERR);
			sect += n;
			nsect -= n;
		} while(nsect);
	}

	/* A FAT volume has been created here */

	/* Determine system ID in the MBR partition table */
	if(FF_FS_EXFAT && fsty == FS_EXFAT) {
		sys = 0x07; // exFAT
	} else if(fsty == FS_FAT32) {
		sys = 0x0C; // FAT32X
	} else if(sz_vol >= 0x10000) {
		sys = 0x06; // FAT12/16 (large)
	} else if(fsty == FS_FAT16) {
		sys = 0x04; // FAT16
	} else {
		sys = 0x01; // FAT12
	}

	/* Update partition information */
	if(FF_MULTI_PARTITION && ipart != 0) { /* Volume is in the existing partition */
		if(!FF_LBA64 || !(fsopt & 0x80)) {
			/* Update system ID in the partition table */
			if(disk_read(pdrv, buf, 0, 1) != RES_OK)
				LEAVE_MKFS(FR_DISK_ERR);							  /* Read the MBR */
			buf[MBR_Table + (ipart - 1) * SZ_PTE + PTE_System] = sys; /* Set system ID */
			if(disk_write(pdrv, buf, 0, 1) != RES_OK)
				LEAVE_MKFS(FR_DISK_ERR); /* Write it back to the MBR */
		}
	} else {					/* Volume as a new single partition */
		if(!(fsopt & FM_SFD)) { /* Create partition table if not in SFD */
			lba[0] = sz_vol;
			lba[1] = 0;
			fr = create_partition(pdrv, lba, sys, buf);
			if(fr != FR_OK)
				LEAVE_MKFS(fr);
		}
	}

	if(disk_ioctl(pdrv, CTRL_SYNC, 0) != RES_OK)
		LEAVE_MKFS(FR_DISK_ERR);

	LEAVE_MKFS(FR_OK);
}

/*-----------------------------------------------------------------------*/
/* Create Partition Table on the Physical Drive                          */
/*-----------------------------------------------------------------------*/
FRESULT f_fdisk(uint8_t pdrv,		/* Physical drive number */
				const LBA_t ptbl[], /* Pointer to the size table for each partitions */
				void* work			/* Pointer to the working buffer (null: use heap memory) */
)
{
	uint8_t* buf = (uint8_t*)work;
	DSTATUS stat;

	stat = disk_initialize(pdrv);
	if(stat & STA_NOINIT)
		return FR_NOT_READY;
	if(stat & STA_PROTECT)
		return FR_WRITE_PROTECTED;
#if FF_USE_LFN == 3
	if(!buf)
		buf = ff_memalloc(FF_MAX_SS); /* Use heap memory for working buffer */
#endif
	if(!buf)
		return FR_NOT_ENOUGH_CORE;

	LEAVE_MKFS(create_partition(pdrv, ptbl, 0x07, buf));
}

} // namespace Storage
