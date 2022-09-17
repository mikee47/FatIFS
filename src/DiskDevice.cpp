#include "include/Storage/DiskDevice.h"
#include "include/Storage/WorkBuffer.h"
#include <Storage/CustomDevice.h>
#include <debug_progmem.h>
#include "diskdefs.h"

extern "C" {
uint32_t os_random();
int os_get_random(void* buf, size_t len);
}

// Definitions from FileSystem
namespace IFS
{
namespace FAT
{
#include "../fatfs/ff.h"
} // namespace FAT
} // namespace IFS

// Minimum number of sectors to switch GPT as partitioning format
#define FF_MIN_GPT 0x10000000

#if FF_MIN_GPT > 0x100000000
#error Wrong FF_MIN_GPT setting
#endif

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

using LBA_t = IFS::FAT::LBA_t;
using FRESULT = IFS::FAT::FRESULT;

/* Find an FAT volume */
/* (It supports only generic partitioning rules, MBR, GPT and SFD) */

/*-----------------------------------------------------------------------*/
/* Create FAT/exFAT volume (with sub-functions)                          */
/*-----------------------------------------------------------------------*/

#if FF_LBA64
/* Create partitions in GPT format */
FRESULT create_partition_gpt(Device& device, unsigned sectorSize,
							 const LBA_t plst[], // Partition list
							 uint8_t sys,		 // System ID (for only MBR, temp setting)
							 WorkBuffer& workBuffer)
{
	auto driveSectors = device.getSectorCount();
	auto partAlignSectors = GPT_ALIGN / sectorSize; // Partition alignment for GPT [sector]
	auto numPartitionTableSectors = GPT_ITEMS * sizeof(gpt_entry_t) / sectorSize; // Size of partition table [sector]
	uint64_t backupPartitionTableSector =
		driveSectors - numPartitionTableSectors - 1;					   // Backup partiiton table start sector
	uint64_t nextAllocatableSector = 2 + numPartitionTableSectors;		   // First allocatable sector
	uint64_t sz_pool = backupPartitionTableSector - nextAllocatableSector; // Size of allocatable area
	uint32_t bcc = 0;													   // Cumulative partition entry checksum
	uint64_t sz_part = 1;
	unsigned partitionIndex = 0; // partition table index
	unsigned si = 0;			 // size table index
	auto entries = workBuffer.as<gpt_entry_t[]>();
	auto entriesPerSector = sectorSize / sizeof(gpt_entry_t);
	for(; partitionIndex < GPT_ITEMS; ++partitionIndex) {
		auto i = partitionIndex % entriesPerSector;
		if(i == 0) {
			workBuffer.clear();
		}

		// Is the size table not terminated?
		if(sz_part != 0) {
			// Align partition start
			nextAllocatableSector = align_up(nextAllocatableSector, partAlignSectors);
			sz_part = plst[si++]; // Get a partition size
			// Is the size in percentage?
			if(sz_part <= 100) {
				sz_part = sz_pool * sz_part / 100;
				// Align partition end
				sz_part = align_up(sz_part, partAlignSectors);
			}
			// Clip the size at end of the pool
			if(nextAllocatableSector + sz_part > backupPartitionTableSector) {
				sz_part = (nextAllocatableSector < backupPartitionTableSector)
							  ? backupPartitionTableSector - nextAllocatableSector
							  : 0;
			}
		}

		// Add a partition?
		if(sz_part != 0) {
			auto& entry = entries[i];
			entry.partition_type_guid = PARTITION_BASIC_DATA_GUID;
			os_get_random(&entry.unique_partition_guid, sizeof(efi_guid_t));
			entry.starting_lba = nextAllocatableSector;
			entry.ending_lba = nextAllocatableSector + sz_part - 1;
			nextAllocatableSector += sz_part; // Next allocatable sector
		}

		// Write the buffer if it is filled up
		if(partitionIndex + 1 == entriesPerSector) {
			// Update cumulative partition entry checksum
			bcc = crc32(bcc, entries, sectorSize);
			// Write to primary table
			auto entryRelativeSector = partitionIndex / entriesPerSector;
			if(!WRITE_SECTORS(entries, 2 + entryRelativeSector, 1)) {
				return IFS::FAT::FR_DISK_ERR;
			}
			// Write to secondary table
			if(!WRITE_SECTORS(entries, backupPartitionTableSector + entryRelativeSector, 1)) {
				return IFS::FAT::FR_DISK_ERR;
			}
		}
	}

	/* Create primary GPT header */
	auto& header = workBuffer.as<gpt_header_t>();
	header = gpt_header_t{
		.signature = GPT_HEADER_SIGNATURE,
		.revision = GPT_HEADER_REVISION_V1,
		.header_size = sizeof(gpt_header_t),
		.my_lba = 1,
		.alternate_lba = driveSectors - 1,
		.first_usable_lba = 2 + numPartitionTableSectors,
		.last_usable_lba = backupPartitionTableSector - 1,
		.partition_entry_lba = 2,
		.num_partition_entries = GPT_ITEMS,
		.sizeof_partition_entry = sizeof(gpt_entry_t),
		.partition_entry_array_crc32 = bcc,
	};
	os_get_random(&header.disk_guid, sizeof(efi_guid_t));
	header.header_crc32 = crc32(&header, sizeof(header));
	if(!WRITE_SECTORS(&header, header.my_lba, 1)) {
		return IFS::FAT::FR_DISK_ERR;
	}

	/* Create secondary GPT header */
	std::swap(header.my_lba, header.alternate_lba);
	header.partition_entry_lba = backupPartitionTableSector;
	header.header_crc32 = 0;
	header.header_crc32 = crc32(&header, sizeof(header));
	if(!WRITE_SECTORS(&header, header.my_lba, 1)) {
		return IFS::FAT::FR_DISK_ERR;
	}

	/* Create protective MBR */
	auto& mbr = workBuffer.as<legacy_mbr_t>();
	mbr = legacy_mbr_t{
		.partition_record = {{
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
		}},
		.signature = MSDOS_MBR_SIGNATURE,
	};
	if(!WRITE_SECTORS(&mbr, 0, 1)) {
		return IFS::FAT::FR_DISK_ERR;
	}

	return IFS::FAT::FR_OK;
}
#endif // FF_LBA64

/* Create partitions in MBR format */
FRESULT create_partition_mbr(Device& device, unsigned sectorSize,
							 const LBA_t plst[], // Partition list
							 uint8_t sys,		 // System ID (for only MBR, temp setting)
							 WorkBuffer& workBuffer)
{
	uint32_t sz_drv32 = device.getSectorCount();
	// Determine drive CHS without any consideration of the drive geometry
	uint8_t n_sc = N_SEC_TRACK;
	uint8_t n_hd;
	for(n_hd = 8; n_hd != 0 && sz_drv32 / (n_hd * n_sc) > 1024; n_hd *= 2) {
	}
	if(n_hd == 0) {
		// Number of heads needs to be < 256
		n_hd = 255;
	}

	workBuffer.clear();
	auto& mbr = workBuffer.as<legacy_mbr_t>();

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
	if(!WRITE_SECTORS(&mbr, 0, 1)) {
		return IFS::FAT::FR_DISK_ERR;
	}

	return IFS::FAT::FR_OK;
}

/* Create partitions on the physical drive in format of MBR or GPT */
FRESULT create_partition(Device& device,
						 const LBA_t plst[], // Partition list
						 uint8_t sys,		 // System ID (for only MBR, temp setting)
						 WorkBuffer& workBuffer)
{
	/* Get physical drive size */
	LBA_t driveSectors = device.getSectorCount();
	if(driveSectors == 0) {
		return IFS::FAT::FR_DISK_ERR;
	}

	auto sectorSize = device.getSectorSize();
	if(sectorSize > FF_MAX_SS || sectorSize < FF_MIN_SS || (sectorSize & (sectorSize - 1)) != 0) {
		return IFS::FAT::FR_DISK_ERR;
	}

#if FF_LBA64
	if(driveSectors >= FF_MIN_GPT) {
		return create_partition_gpt(device, sectorSize, plst, sys, workBuffer);
	}
#endif

	return create_partition_mbr(device, sectorSize, plst, sys, workBuffer);
}

#ifdef ENABLE_EXFAT
/*
 * Create a compressed up-case table
 */
FRESULT createUpcaseTable(Device& device, LBA_t sect, unsigned sectorSize, uint32_t& szb_case, uint32_t& sum,
						  WorkBuffer& workBuffer)
{
	szb_case = 0;
	sum = 0; // Table checksum to be stored in the 82 entry
	unsigned st = 0;
	IFS::FAT::WCHAR si = 0;
	unsigned sectorOffset = 0;
	unsigned j = 0;
	do {
		IFS::FAT::WCHAR ch;
		switch(st) {
		case 0: {
			ch = IFS::FAT::ff_wtoupper(si);
			if(ch != si) {
				// Store the up-case char if exist
				++si;
				break;
			}
			// Get run length of no-case block
			ch = si + 1;
			while(ch != 0 && ch == IFS::FAT::ff_wtoupper(ch)) {
				++ch;
			}
			j = ch - si;
			// Compress the no-case block if run is >= 128 chars
			if(j >= 128) {
				ch = 0xFFFF;
				st = 2;
				break;
			}
			// Do not compress short run
			st = 1;
			/* FALLTHROUGH */
		}

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
		workBuffer[sectorOffset++] = ch;
		sum = xsum32(ch, sum);
		workBuffer[sectorOffset++] = ch >> 8;
		sum = xsum32(ch >> 8, sum);
		szb_case += 2;

		// Write buffered data when buffer full or end of process
		if(si == 0 || sectorOffset == workBuffer.size()) {
			auto n = getBlockCount(sectorOffset, sectorSize);
			if(!WRITE_SECTORS(workBuffer.get(), sect, n)) {
				return IFS::FAT::FR_DISK_ERR;
			}
			sect += n;
			sectorOffset = 0;
		}
	} while(si != 0);

	return IFS::FAT::FR_OK;
}

FRESULT createExFatVolume(Device& device, uint16_t sectorSize, WorkBuffer& workBuffer, LBA_t volumeStartSector,
						  LBA_t volumeSectorCount, uint32_t sectorsPerCluster, uint32_t sectorsPerBlock, uint32_t vsn)
{
	if(volumeSectorCount < 0x1000) {
		// Volume too small for exFAT
		return IFS::FAT::FR_MKFS_ABORTED;
	}
#if FF_USE_TRIM
	// Inform storage device that the volume area may be erased
	device.trim(volumeStartSector, volumeSectorCount);
#endif
	/* Determine FAT location, data location and number of clusters */
	if(sectorsPerCluster == 0) { /* AU auto-selection */
		if(volumeSectorCount >= 64 * 1024 * 1024) {
			sectorsPerCluster = 256;
		} else if(volumeSectorCount >= 512 * 1024) {
			sectorsPerCluster = 64;
		} else {
			sectorsPerCluster = 8;
		}
	}
	const LBA_t fatStartSector = volumeStartSector + 32;
	const uint32_t numFatSectors =
		getBlockCount((volumeSectorCount / sectorsPerCluster + 2) * sizeof(uint32_t), sectorSize);
	const LBA_t dataStartSector = align_up(fatStartSector + numFatSectors, sectorsPerBlock);
	if(dataStartSector - volumeStartSector >= volumeSectorCount / 2) {
		// Volume too small
		return IFS::FAT::FR_MKFS_ABORTED;
	}
	const uint32_t numClusters = (volumeSectorCount - (dataStartSector - volumeStartSector)) / sectorsPerCluster;
	if(numClusters < 16 || numClusters > MAX_EXFAT) {
		// Too few/many clusters
		return IFS::FAT::FR_MKFS_ABORTED;
	}

	const uint32_t bitmapSize = (numClusters + 7) / 8; // Size of allocation bitmap

	// bitmap, upcaseTable, rootDir
	uint32_t clusterLengths[3] = {
		getBlockCount(bitmapSize, sectorsPerCluster * sectorSize), // Number of allocation bitmap clusters
	};

	/* Create a compressed up-case table */
	uint32_t szb_case;
	uint32_t sum_case;
	LBA_t sect = dataStartSector + sectorsPerCluster * clusterLengths[0]; // Table start sector
	FRESULT fr = createUpcaseTable(device, sect, sectorSize, szb_case, sum_case, workBuffer);
	if(fr) {
		return fr;
	}

	clusterLengths[1] = getBlockCount(szb_case, sectorsPerCluster * sectorSize); // Number of up-case table clusters
	clusterLengths[2] = 1;														 // Number of root dir clusters

	/* Initialize the allocation bitmap */
	sect = dataStartSector;
	auto nsect = getBlockCount(bitmapSize, sectorSize); // Start of bitmap and number of bitmap sectors
	auto nbit = clusterLengths[0] + clusterLengths[1] +
				clusterLengths[2]; // Number of clusters in-use by system (bitmap, up-case and root-dir)
	do {
		workBuffer.clear();

		// Mark used clusters
		auto maxBits = 8 * workBuffer.size();
		for(unsigned i = 0; i < maxBits && nbit != 0; ++i, --nbit) {
			workBuffer[i / 8] |= 1 << (i % 8);
		}
		auto n = std::min(nsect, workBuffer.sectors());
		if(!WRITE_SECTORS(workBuffer.get(), sect, n)) {
			return IFS::FAT::FR_DISK_ERR;
		}
		sect += n;
		nsect -= n;
	} while(nsect != 0);

	/* Initialize the FAT */
	sect = fatStartSector;
	nsect = numFatSectors; /* Start of FAT and number of FAT sectors */
	unsigned chainIndex{0};
	nbit = 0;
	uint32_t clu = 0;
	do {
		workBuffer.clear();
		auto fat = workBuffer.as<uint32_t[]>();
		auto clusterCount = workBuffer.size() / sizeof(uint32_t);
		if(clu == 0) {
			fat[0] = 0xFFFFFFF8;
			fat[1] = 0xFFFFFFFF;
			clu = 2;
		}
		do {
			/* Create chains of bitmap, up-case and root dir */
			for(; nbit != 0 && clu < clusterCount; ++clu, --nbit) {
				fat[clu] = (nbit > 1) ? clu + 1 : 0xFFFFFFFF;
			}
			if(nbit == 0 && chainIndex < 3) {
				nbit = clusterLengths[chainIndex++]; // Next chain length
			}
		} while(nbit != 0 && clu < clusterCount);
		auto n = std::min(nsect, workBuffer.sectors());
		if(!WRITE_SECTORS(fat, sect, n)) {
			return IFS::FAT::FR_DISK_ERR;
		}
		sect += n;
		nsect -= n;
	} while(nsect != 0);

	/* Initialize the root directory */
	workBuffer.clear();
	auto dir = workBuffer.as<EXFAT::exfat_dentry_t[]>();
	// Volume label entry (no label)
	dir[0].type = EXFAT_VOLUME;
	// Bitmap entry
	dir[1].type = EXFAT_BITMAP;
	dir[1].bitmap.start_clu = 2;
	dir[1].bitmap.size = bitmapSize;
	// Up-case table entry
	dir[2].type = EXFAT_UPCASE;
	dir[2].upcase.checksum = sum_case;
	dir[2].upcase.start_clu = 2 + clusterLengths[0];
	dir[2].upcase.size = szb_case;

	sect = dataStartSector + sectorsPerCluster * (clusterLengths[0] + clusterLengths[1]);
	nsect = sectorsPerCluster; /* Start of the root directory and number of sectors */
	do {					   /* Fill root directory sectors */
		auto n = std::min(nsect, workBuffer.sectors());
		if(!WRITE_SECTORS(dir, sect, n)) {
			return IFS::FAT::FR_DISK_ERR;
		}
		// Rest of entries are filled with zero
		workBuffer.clear();
		sect += n;
		nsect -= n;
	} while(nsect != 0);

	/* Create two sets of the exFAT VBR blocks */
	sect = volumeStartSector;
	for(unsigned n = 0; n < 2; ++n) {
		/* Main record (+0) */
		workBuffer.clear();
		auto& bpb = workBuffer.as<EXFAT::boot_sector_t>();
		bpb = EXFAT::boot_sector_t{
			.jmp_boot = {0xEB, 0x76, 0x90},
			.fs_type = FSTYPE_EXFAT,
			.partition_offset = volumeStartSector,						 // Volume offset in the physical drive [sector]
			.vol_length = volumeSectorCount,							 // Volume size [sector]
			.fat_offset = uint32_t(fatStartSector - volumeStartSector),  // FAT offset [sector]
			.fat_length = numFatSectors,								 // FAT size [sector]
			.clu_offset = uint32_t(dataStartSector - volumeStartSector), // Data offset [sector]
			.clu_count = numClusters,									 // Number of clusters
			.root_cluster = 2 + clusterLengths[0] + clusterLengths[1],   // Root dir cluster #
			.vol_serial = vsn,
			.fs_revision = 0x0100, // Filesystem version (1.00)
			.sect_size_bits = getSizeBits(sectorSize),
			.sect_per_clus_bits = getSizeBits(sectorsPerCluster),
			.num_fats = 1,
			.drv_sel = 0x80,		   // Drive number (for int13)
			.boot_code = {0xEB, 0xFE}, // Boot code (x86)
			.signature = BOOT_SIGNATURE,
		};

		// Calculate VBR checksum
		// NOTE: vol_flags and percent_in_use NOT included
		uint32_t sum = 0;
		for(unsigned i = 0; i < sectorSize; ++i) {
			if(i == offsetof(EXFAT::boot_sector_t, vol_flags) || i == offsetof(EXFAT::boot_sector_t, vol_flags) + 1) {
				continue;
			}
			if(i == offsetof(EXFAT::boot_sector_t, percent_in_use)) {
				continue;
			}
			sum = xsum32(workBuffer[i], sum);
		}
		if(!WRITE_SECTORS(&bpb, sect++, 1)) {
			return IFS::FAT::FR_DISK_ERR;
		}
		/* Extended bootstrap record (+1..+8) */
		workBuffer.clear();
		bpb.signature = BOOT_SIGNATURE;
		unsigned sectorIndex = 1;
		for(; sectorIndex < 9; ++sectorIndex) {
			sum = xsum32(&bpb, sectorSize, sum);
			if(!WRITE_SECTORS(&bpb, sect++, 1)) {
				return IFS::FAT::FR_DISK_ERR;
			}
		}
		/* OEM/Reserved record (+9..+10) */
		workBuffer.clear();
		for(; sectorIndex < 11; sectorIndex++) {
			sum = xsum32(workBuffer.get(), sectorSize, sum);
			if(!WRITE_SECTORS(workBuffer.get(), sect++, 1)) {
				return IFS::FAT::FR_DISK_ERR;
			}
		}
		/* Fill sum record (+11) with checksum value */
		auto sumRecord = workBuffer.as<uint32_t[]>();
		std::fill_n(sumRecord, sectorSize / sizeof(uint32_t), sum);
		if(!WRITE_SECTORS(sumRecord, sect++, 1)) {
			return IFS::FAT::FR_DISK_ERR;
		}
	}

	return IFS::FAT::FR_OK;
}

#endif // ENABLE_EXFAT

FRESULT createFatVolume(Device& device, uint16_t sectorSize, WorkBuffer& workBuffer, LBA_t volumeStartSector,
						LBA_t volumeSectorCount, uint32_t sectorsPerCluster, uint32_t sectorsPerBlock, uint32_t vsn,
						int fsty, const bool allowFat32, const unsigned numRootEntries, const uint8_t numFats)
{
	uint32_t pau;			// Physical Allocation Unit
	uint32_t numClusters;   // Number of clusters
	uint32_t numFatSectors; // FAT size [sector]
	uint32_t sz_rsv;		// Number of reserved sectors
	uint32_t sz_dir;		// Root dir size [sector]
	LBA_t fatStartSector;
	do {
		pau = sectorsPerCluster;
		/* Pre-determine number of clusters and FAT sub-type */
		if(fsty == FS_FAT32) {
			/* FAT32 volume */
			if(pau == 0) {
				// Determine allocation unit size from Volume size, in unit of 128K
				auto n = volumeSectorCount / 0x20000;
				static const uint16_t cst[] = {1, 2, 4, 8, 16, 32, 0};
				for(unsigned i = 0, pau = 1; cst[i] != 0 && cst[i] <= n; ++i, pau <<= 1) {
				}
			}
			numClusters = volumeSectorCount / pau;
			numFatSectors = getBlockCount((2 + numClusters) * sizeof(uint32_t), sectorSize);
			sz_rsv = 32;
			sz_dir = 0; // No static directory
			if(numClusters <= MAX_FAT16 || numClusters > MAX_FAT32) {
				return IFS::FAT::FR_MKFS_ABORTED;
			}
		} else {
			/* FAT volume */
			uint32_t n; // FAT size [byte]
			if(pau == 0) {
				// Determine allocation unit size from volume size, in units of 4K
				n = volumeSectorCount / 0x1000;
				static const uint16_t cst[] = {1, 4, 16, 64, 256, 512, 0};
				for(unsigned i = 0, pau = 1; cst[i] != 0 && cst[i] <= n; ++i, pau <<= 1) {
				}
			}
			numClusters = volumeSectorCount / pau;
			if(numClusters > MAX_FAT12) {
				n = numClusters * 2 + 4;
			} else {
				fsty = FS_FAT12;
				n = (numClusters * 3 + 1) / 2 + 3;
			}
			numFatSectors = (n + sectorSize - 1) / sectorSize;
			sz_rsv = 1;
			sz_dir = numRootEntries * sizeof(FAT::msdos_dir_entry_t) / sectorSize;
		}
		fatStartSector = volumeStartSector + sz_rsv;							   // FAT base sector
		LBA_t dataStartSector = fatStartSector + numFatSectors * numFats + sz_dir; // Data base sector

		/* Align data area to erase block boundary (for flash memory media) */
		uint32_t n = align_up(dataStartSector, sectorsPerBlock) -
					 dataStartSector; // Sectors to next nearest from current data base
		if(fsty == FS_FAT32) {		  /* FAT32: Move FAT */
			sz_rsv += n;
			fatStartSector += n;
		} else { /* FAT: Expand FAT */
			if(n % numFats != 0) {
				// Adjust fractional error
				--n;
				++sz_rsv;
				++fatStartSector;
			}
			numFatSectors += n / numFats;
		}

		/* Determine number of clusters and final check of validity of the FAT sub-type */
		if(volumeSectorCount < dataStartSector + pau * 16 - volumeStartSector) {
			// Volume too small
			return IFS::FAT::FR_MKFS_ABORTED;
		}
		numClusters = (volumeSectorCount - sz_rsv - numFatSectors * numFats - sz_dir) / pau;
		if(fsty == FS_FAT32) {
			if(numClusters <= MAX_FAT16) {
				// Too few clusters for FAT32
				if(sectorsPerCluster == 0) {
					// Adjust cluster size and retry
					sectorsPerCluster = pau / 2;
					if(sectorsPerCluster != 0) {
						continue;
					}
				}
				return IFS::FAT::FR_MKFS_ABORTED;
			}
		}

		if(fsty == FS_FAT16) {
			if(numClusters > MAX_FAT16) {
				// Too many clusters for FAT16
				if(sectorsPerCluster == 0 && (pau * 2) <= 64) {
					// Adjust cluster size and retry
					sectorsPerCluster = pau * 2;
					continue;
				}
				if(allowFat32) {
					// Switch type to FAT32 and retry
					fsty = FS_FAT32;
					continue;
				}
				if(sectorsPerCluster == 0) {
					// Adjust cluster size and retry
					sectorsPerCluster = pau * 2;
					if(sectorsPerCluster <= 128) {
						continue;
					}
				}
				return IFS::FAT::FR_MKFS_ABORTED;
			}

			if(numClusters <= MAX_FAT12) {
				// Too few clusters for FAT16
				if(sectorsPerCluster == 0) {
					// Adjust cluster size and retry
					sectorsPerCluster = pau * 2;
					if(sectorsPerCluster <= 128) {
						continue;
					}
				}
				return IFS::FAT::FR_MKFS_ABORTED;
			}
		}

		if(fsty == FS_FAT12 && numClusters > MAX_FAT12) {
			// Too many clusters for FAT12
			return IFS::FAT::FR_MKFS_ABORTED;
		}

		// Ok, cluster configuration is valid
		break;
	} while(true);

#if FF_USE_TRIM
	// Inform storage device that the volume area may be erased
	device.trim(volumeStartSector, volumeSectorCount);
#endif
	/* Create FAT VBR */
	workBuffer.clear();
	auto& bpb = workBuffer.as<FAT::fat_boot_sector_t>();
	bpb = FAT::fat_boot_sector_t{
		.jmp_boot = {0xEB, 0xFE, 0x90},
		.system_id = {'M', 'S', 'D', 'O', 'S', '5', '.', '0'},
		.sector_size = sectorSize,
		.sec_per_clus = uint8_t(pau),
		.reserved = uint16_t(sz_rsv),
		.num_fats = uint8_t(numFats),
		.dir_entries = uint16_t((fsty == FS_FAT32) ? 0 : numRootEntries),
		.sectors = uint16_t((volumeSectorCount < 0x10000) ? volumeSectorCount : 0), // Volume size in 16-bit LBA
		.media = 0xF8,
		.fat_length = uint16_t((fsty != FS_FAT32) ? numFatSectors : 0),
		.secs_track = 64,					   // Number of sectors per track (for int13)
		.heads = 255,						   // Number of heads (for int13)
		.hidden = uint32_t(volumeStartSector), // Volume offset in the physical drive [sector]
		.total_sect = uint32_t((volumeSectorCount >= 0x10000) ? volumeSectorCount : 0), // Volume size in 32-bit LBA
	};
	if(fsty == FS_FAT32) {
		bpb.fat32 = decltype(bpb.fat32){
			.fat_length = numFatSectors,
			.root_cluster = 2,
			.info_sector = 1,	 // Offset of FSINFO sector (VBR + 1)
			.backup_boot = 6,	 // Offset of backup VBR (VBR + 6)
			.drive_number = 0x80, // Drive number (for int13)
			.signature = 0x29,	// Extended boot signature
			.vol_id = vsn,
			.vol_label = {'N', 'O', ' ', 'N', 'A', 'M', 'E', ' ', ' ', ' ', ' '},
			.fs_type = FSTYPE_FAT32,
		};
	} else {
		bpb.fat16 = decltype(bpb.fat16){
			.drive_number = 0x80, // Drive number (for int13)
			.signature = 0x29,	// Extended boot signature
			.vol_id = vsn,
			.vol_label = {'N', 'O', ' ', 'N', 'A', 'M', 'E', ' ', ' ', ' ', ' '},
			.fs_type = FSTYPE_FAT,
		};
	}
	bpb.signature = BOOT_SIGNATURE;
	if(!WRITE_SECTORS(&bpb, volumeStartSector, 1)) {
		return IFS::FAT::FR_DISK_ERR;
	}

	/* Create FSINFO record if needed */
	if(fsty == FS_FAT32) {
		/* Write backup VBR (VBR + 6) */
		if(!WRITE_SECTORS(&bpb, volumeStartSector + 6, 1)) {
			return IFS::FAT::FR_DISK_ERR;
		}
		workBuffer.clear();
		auto& fsinfo = workBuffer.as<FAT::fat_boot_fsinfo_t>();
		fsinfo = FAT::fat_boot_fsinfo_t{
			.signature1 = FAT_FSINFO_SIG1,
			.signature2 = FAT_FSINFO_SIG2,
			.free_clusters = numClusters - 1,
			.next_cluster = 2,
			.signature = BOOT_SIGNATURE,
		};
		// Write backup FSINFO (VBR + 7)
		if(!WRITE_SECTORS(&fsinfo, volumeStartSector + 7, 1)) {
			return IFS::FAT::FR_DISK_ERR;
		}
		// Write original FSINFO (VBR + 1)
		if(!WRITE_SECTORS(&fsinfo, volumeStartSector + 1, 1)) {
			return IFS::FAT::FR_DISK_ERR;
		}
	}

	/* Initialize FAT area */
	workBuffer.clear();
	auto sect = fatStartSector;
	for(unsigned i = 0; i < numFats; ++i) {
		// Initialize each FAT
		if(fsty == FS_FAT32) {
			auto fat = workBuffer.as<uint32_t[]>();
			fat[0] = 0xFFFFFFF8;
			fat[1] = 0xFFFFFFFF;
			fat[2] = 0x0FFFFFFF; // root directory
		} else {
			auto fat = workBuffer.as<uint16_t[]>();
			fat[0] = 0xFFF8;
			fat[1] = (fsty == FS_FAT12) ? 0x00FF : 0xFFFF;
		}
		for(auto nsect = numFatSectors; nsect != 0;) {
			// Fill FAT sectors
			auto n = std::min(nsect, workBuffer.sectors());
			if(!WRITE_SECTORS(workBuffer.get(), sect, n)) {
				return IFS::FAT::FR_DISK_ERR;
			}
			// Rest of FAT is empty
			workBuffer.clear();
			sect += n;
			nsect -= n;
		}
	}

	/* Initialize root directory (fill with zero) */
	for(auto nsect = (fsty == FS_FAT32) ? pau : sz_dir; nsect != 0;) {
		auto n = std::min(nsect, workBuffer.sectors());
		if(!WRITE_SECTORS(workBuffer.get(), sect, n)) {
			return IFS::FAT::FR_DISK_ERR;
		}
		sect += n;
		nsect -= n;
	}

	return IFS::FAT::FR_OK;
}

} // namespace

FRESULT f_mkfs(Device& device, Storage::MKFS_PARM opt)
{
	uint8_t fsty;

	/* Get physical drive status (sz_drv, sectorsPerBlock, sectorSize) */
	uint16_t sectorSize;
#if FF_MAX_SS != FF_MIN_SS
	sectorSize = device.getSectorSize();
	if(sectorSize > FF_MAX_SS || sectorSize < FF_MIN_SS || (sectorSize & (sectorSize - 1))) {
		return IFS::FAT::FR_DISK_ERR;
	}
#else
	sectorSize = FF_MAX_SS;
#endif
	uint32_t sectorsPerBlock = opt.align ?: device.getBlockSize() / sectorSize;
	if(sectorsPerBlock == 0 || sectorsPerBlock > 0x8000 || (sectorsPerBlock & (sectorsPerBlock - 1))) {
		sectorsPerBlock = 1;
	}

	/* Options for FAT sub-type and FAT parameters */
	bool useGpt{false};
	uint8_t numFats = (opt.numFats >= 2) ? 2 : 1;
	unsigned numRootEntries = opt.numRootEntries;
	if(numRootEntries == 0 || numRootEntries > 0x8000 ||
	   (numRootEntries % (sectorSize / sizeof(EXFAT::exfat_dentry_t))) != 0) {
		numRootEntries = 512;
	}

	uint32_t sectorsPerCluster;
	if(opt.clusterSize == 0) {
		sectorsPerCluster = 0; // Auto-detect later on
	} else {
		// Round up requested cluster size to nearest power of 2
		auto clusterSize = 0x80000000U >> __builtin_clz(opt.clusterSize);
		clusterSize = std::max(0x1000000U, clusterSize);
		sectorsPerCluster = clusterSize / sectorSize;
	}

	/* Get working buffer */
	WorkBuffer workBuffer(sectorSize, 1);
	if(!workBuffer) {
		return IFS::FAT::FR_NOT_ENOUGH_CORE;
	}

	/* Determine where the volume to be located (volumeStartSector, volumeSectorCount) */
	LBA_t volumeStartSector = 0;
	LBA_t volumeSectorCount = 0;		   // Size of volume
	if(FF_MULTI_PARTITION && ipart != 0) { /* Is the volume associated with any specific partition? */
		/* Get partition location from the existing partition table */

		// Load MBR
		auto& mbr = workBuffer.as<legacy_mbr_t>();
		if(!READ_SECTORS(&mbr, 0, 1)) {
			return IFS::FAT::FR_DISK_ERR;
		}
		if(mbr.signature != MSDOS_MBR_SIGNATURE) {
			return IFS::FAT::FR_MKFS_ABORTED;
		}
#if FF_LBA64
		// GPT protective MBR?
		if(mbr.partition_record[0].os_type == EFI_PMBR_OSTYPE_EFI_GPT) {
			/* Get the partition location from GPT */
			if(!READ_SECTORS(workBuffer.get(), 1, 1)) {
				return IFS::FAT::FR_DISK_ERR;
			}
			auto& gpt = workBuffer.as<gpt_header_t>();
			if(!verifyGptHeader(gpt)) {
				return IFS::FAT::FR_MKFS_ABORTED;
			}
			auto n_ent = gpt.num_partition_entries;
			auto pt_lba = gpt.partition_entry_lba;
			unsigned i = 0;
			auto entries = workBuffer.as<gpt_entry_t[]>();
			auto entriesPerSector = sectorSize / sizeof(gpt_entry_t);
			/* Find MS Basic partition with order of ipart */
			for(unsigned iEntry = 0; iEntry < n_ent; ++iEntry) {
				if(iEntry % entriesPerSector == 0) {
					if(!READ_SECTORS(entries, pt_lba++, 1)) {
						return IFS::FAT::FR_DISK_ERR;
					}
				}
				auto& entry = entries[iEntry % entriesPerSector];
				if(entry.partition_type_guid != PARTITION_BASIC_DATA_GUID) {
					continue;
				}
				++i;
				if(i < ipart) {
					continue;
				}
				volumeStartSector = entry.starting_lba;
				volumeSectorCount = 1 + entry.ending_lba - entry.starting_lba;
				break;
			}
			if(n_ent == 0) {
				// Partition not found
				return IFS::FAT::FR_MKFS_ABORTED;
			}
			useGpt = true;
		} else
#endif
		{
			/* Get the partition location from MBR partition table */
			auto& entry = mbr.partition_record[ipart - 1];
			if(ipart > 4 || entry.os_type == 0) {
				return IFS::FAT::FR_MKFS_ABORTED; // No partition
			}
			volumeStartSector = entry.starting_lba;
			volumeSectorCount = entry.size_in_lba;
		}
	} else {
		/* The volume is associated with a physical drive */
		volumeSectorCount = device.getSectorCount();
		// To be partitioned?
		if(opt.createPartition) {
			// Create a single-partition on the drive
#if FF_LBA64
			// Decide on MBR/GPT partition type
			if(volumeSectorCount >= FF_MIN_GPT) {
				useGpt = true;
				volumeStartSector = GPT_ALIGN / sectorSize;
				// Estimate partition offset and size
				volumeSectorCount -= 1 + volumeStartSector + GPT_ITEMS * sizeof(gpt_entry_t) / sectorSize;
			} else
#endif
			{
				// Partitioning is in MBR
				if(volumeSectorCount > N_SEC_TRACK) {
					// Estimate partition offset and size
					volumeStartSector = N_SEC_TRACK;
					volumeSectorCount -= volumeStartSector;
				}
			}
		}
	}
	// Check for minimum volume size
	if(volumeSectorCount < 128) {
		return IFS::FAT::FR_MKFS_ABORTED;
	}

	/* Now start to create a FAT volume at volumeStartSector and volumeSectorCount */

	do {
		/* Pre-determine the FAT type */
#ifdef ENABLE_EXFAT
		// exFAT only, vol >= 64MS or sectorsPerCluster > 128S ?
		if(opt.types == DiskPart::Type::exfat || volumeSectorCount >= 0x4000000 || sectorsPerCluster > 128) {
			fsty = FS_EXFAT;
			break;
		}
#endif
#if FF_LBA64
		// FAT32 volumes limited to 4GB
		if(volumeSectorCount >> 32 != 0) {
			return IFS::FAT::FR_MKFS_ABORTED;
		}
#endif
		// Ensure AU is valid for FAT/FAT32
		if(sectorsPerCluster > 128) {
			sectorsPerCluster = 128;
		}
		if(opt.types == DiskPart::Type::fat32) {
			fsty = FS_FAT32;
			break;
		}
		if(!opt.types[DiskPart::Type::fat]) {
			return IFS::FAT::FR_INVALID_PARAMETER;
		}
		fsty = FS_FAT16;
	} while(0);

	uint32_t vsn = os_random();

#ifdef ENABLE_EXFAT
	if(fsty == FS_EXFAT) {
		/* Create an exFAT volume */
		createExFatVolume(device, sectorSize, workBuffer, volumeStartSector, volumeSectorCount, sectorsPerCluster,
						  sectorsPerBlock, vsn);

	} else
#endif
	{
		createFatVolume(device, sectorSize, workBuffer, volumeStartSector, volumeSectorCount, sectorsPerCluster,
						sectorsPerBlock, vsn, fsty, opt.types[DiskPart::Type::fat32], numRootEntries, numFats);
	}

	/* A FAT volume has been created here */

	/* Determine system ID in the MBR partition table */
	uint8_t sys;
	if(fsty == FS_EXFAT) {
		// exFAT
		sys = 0x07;
	} else if(fsty == FS_FAT32) {
		// FAT32X
		sys = 0x0C;
	} else if(volumeSectorCount >= 0x10000) {
		// FAT12/16 (large)
		sys = 0x06;
	} else if(fsty == FS_FAT16) {
		sys = 0x04;
	} else {
		sys = 0x01;
	}

	/* Update partition information */
	if(FF_MULTI_PARTITION && ipart != 0) { /* Volume is in the existing partition */
		if(!useGpt) {
			/* Update system ID in the partition table (MBR) */
			auto& mbr = workBuffer.as<legacy_mbr_t>();
			if(!READ_SECTORS(&mbr, 0, 1)) {
				return IFS::FAT::FR_DISK_ERR;
			}
			mbr.partition_record[ipart - 1].os_type = sys;
			if(!WRITE_SECTORS(&mbr, 0, 1)) {
				return IFS::FAT::FR_DISK_ERR;
			}
		}
	} else if(opt.createPartition) {
		// Not in SFD: create partition table
		LBA_t lba[] = {volumeSectorCount, 0};
		auto fr = create_partition(device, lba, sys, workBuffer);
		if(fr != IFS::FAT::FR_OK) {
			return fr;
		}
	}

	if(!device.sync()) {
		return IFS::FAT::FR_DISK_ERR;
	}

	return IFS::FAT::FR_OK;
}

} // namespace Storage
