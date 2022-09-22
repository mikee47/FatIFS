#include "include/Storage/DiskDevice.h"
#include "include/Storage/WorkBuffer.h"
#include <Storage/CustomDevice.h>
#include <IFS/TimeStamp.h>
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

using LBA_t = IFS::FAT::LBA_t;
using FRESULT = IFS::FAT::FRESULT;

#define READ_SECTORS(buff, sector, count) device.readSectors(sectorSizeShift, buff, sector, count)
#define WRITE_SECTORS(buff, sector, count) device.writeSectors(sectorSizeShift, buff, sector, count)

/* Find an FAT volume */
/* (It supports only generic partitioning rules, MBR, GPT and SFD) */

/*-----------------------------------------------------------------------*/
/* Create FAT/exFAT volume (with sub-functions)                          */
/*-----------------------------------------------------------------------*/

#if FF_LBA64
/* Create partitions in GPT format */
FRESULT create_partition_gpt(Device& device, unsigned sectorSizeShift,
							 const LBA_t plst[], // Partition list
							 WorkBuffer& workBuffer)
{
	auto sectorSize = 1U << sectorSizeShift;
	auto driveSectors = device.getSectorCount();
	auto partAlignSectors = GPT_ALIGN >> sectorSizeShift; // Partition alignment for GPT [sector]
	auto numPartitionTableSectors =
		GPT_ITEMS * sizeof(gpt_entry_t) >> sectorSizeShift; // Size of partition table [sector]
	uint64_t backupPartitionTableSector =
		driveSectors - numPartitionTableSectors - 1;					   // Backup partition table start sector
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
FRESULT create_partition_mbr(Device& device, unsigned sectorSizeShift,
							 const LBA_t plst[], // Partition list
							 DiskPart::SysIndicator sys, WorkBuffer& workBuffer)
{
	uint32_t numDeviceSectors = device.getSectorCount();
	// Determine drive CHS without any consideration of the drive geometry
	constexpr uint8_t sectorsPerTrack = N_SEC_TRACK;
	uint8_t numHeads;
	for(numHeads = 8; numHeads != 0 && numDeviceSectors / (numHeads * sectorsPerTrack) > 1024; numHeads *= 2) {
	}
	if(numHeads == 0) {
		// Number of heads needs to be < 256
		numHeads = 255;
	}

	workBuffer.clear();
	auto& mbr = workBuffer.as<legacy_mbr_t>();

	uint32_t sect = sectorsPerTrack;
	for(unsigned i = 0; i < 4 && sect != 0 && sect < numDeviceSectors; ++i) {
		uint32_t numPartSectors = plst[i]; // Get partition size
		if(numPartSectors <= 100) {
			// Size as percentage
			numPartSectors = (numPartSectors == 100) ? numDeviceSectors : numPartSectors * (numDeviceSectors / 100);
		}
		if(sect + numPartSectors > numDeviceSectors || sect + numPartSectors < sect) {
			// Clip at drive size
			numPartSectors = numDeviceSectors - sect;
		}
		if(numPartSectors == 0) {
			// End of table or no sector to allocate
			break;
		}

		struct CHS {
			uint8_t head;
			uint8_t sector;
			uint8_t track;
		};

		auto calc_CHS = [&](uint32_t sect) -> CHS {
			unsigned tracks = sect / sectorsPerTrack;
			uint8_t sec = 1 + (sect % sectorsPerTrack);
			unsigned cyl = tracks / numHeads;
			uint8_t head = tracks % numHeads;
			sec |= (cyl >> 2) & 0xC0;
			return CHS{head, sec, uint8_t(cyl)};
		};

		auto start = calc_CHS(sect);
		auto end = calc_CHS(sect + numPartSectors - 1);

		mbr.partition_record[i] = gpt_mbr_record_t{
			.start_head = start.head,
			.start_sector = start.sector,
			.start_track = start.track,
			.os_type = sys,
			.end_head = end.head,
			.end_sector = end.sector,
			.end_track = end.track,
			.starting_lba = sect,
			.size_in_lba = numPartSectors,
		};

		sect += numPartSectors;
	}

	mbr.signature = MSDOS_MBR_SIGNATURE;
	return WRITE_SECTORS(&mbr, 0, 1) ? IFS::FAT::FR_OK : IFS::FAT::FR_DISK_ERR;
}

/* Create partitions on the physical drive in format of MBR or GPT */
FRESULT create_partition(Device& device,
						 const LBA_t plst[], // Partition list
						 DiskPart::SysIndicator sys, WorkBuffer& workBuffer)
{
	/* Get physical drive size */
	LBA_t driveSectors = device.getSectorCount();
	if(driveSectors == 0) {
		return IFS::FAT::FR_DISK_ERR;
	}

	auto sectorSize = device.getSectorSize();
	uint8_t sectorSizeShift = getSizeBits(sectorSize);
	if(sectorSize > FF_MAX_SS || sectorSize < FF_MIN_SS || sectorSize != (1U << sectorSizeShift)) {
		return IFS::FAT::FR_DISK_ERR;
	}

#if FF_LBA64
	if(driveSectors >= FF_MIN_GPT) {
		return create_partition_gpt(device, sectorSizeShift, plst, workBuffer);
	}
#endif

	return create_partition_mbr(device, sectorSizeShift, plst, sys, workBuffer);
}

#ifdef ENABLE_EXFAT
/*
 * Create a compressed up-case table
 */
FRESULT createUpcaseTable(Device& device, LBA_t sect, unsigned sectorSizeShift, uint32_t& szb_case, uint32_t& sum,
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
			auto n = getBlockCount(sectorOffset, 1U << sectorSizeShift);
			if(!WRITE_SECTORS(workBuffer.get(), sect, n)) {
				return IFS::FAT::FR_DISK_ERR;
			}
			sect += n;
			sectorOffset = 0;
		}
	} while(si != 0);

	return IFS::FAT::FR_OK;
}

FRESULT createExFatVolume(Device& device, const FatParam& param)
{
	if(param.volumeSectorCount < 0x1000) {
		// Volume too small for exFAT
		return IFS::FAT::FR_MKFS_ABORTED;
	}
#if FF_USE_TRIM
	// Inform storage device that the volume area may be erased
	device.trim(param.volumeStartSector, param.volumeSectorCount);
#endif
	/* Determine FAT location, data location and number of clusters */
	auto sectorSizeShift = param.sectorSizeShift;
	auto sectorSize = 1U << sectorSizeShift;
	auto sectorsPerCluster = param.sectorsPerCluster;
	if(sectorsPerCluster == 0) {
		// Auto-select cluster size
		if(param.volumeSectorCount >= 64 * 1024 * 1024) {
			sectorsPerCluster = 256;
		} else if(param.volumeSectorCount >= 512 * 1024) {
			sectorsPerCluster = 64;
		} else {
			sectorsPerCluster = 8;
		}
	}
	const LBA_t fatStartSector = param.volumeStartSector + 32;
	const uint32_t numFatSectors =
		getBlockCount((param.volumeSectorCount / sectorsPerCluster + 2) * sizeof(uint32_t), sectorSize);
	const LBA_t dataStartSector = align_up(fatStartSector + numFatSectors, param.sectorsPerBlock);
	if(dataStartSector - param.volumeStartSector >= param.volumeSectorCount / 2) {
		// Volume too small
		return IFS::FAT::FR_MKFS_ABORTED;
	}
	const uint32_t numClusters =
		(param.volumeSectorCount - (dataStartSector - param.volumeStartSector)) / sectorsPerCluster;
	if(numClusters < 16 || numClusters > MAX_EXFAT) {
		// Too few/many clusters
		return IFS::FAT::FR_MKFS_ABORTED;
	}

	const uint32_t bitmapSize = (numClusters + 7) / 8; // Size of allocation bitmap

	// Number of clusters for bitmap, upcaseTable, rootDir
	uint32_t clusterLengths[3] = {
		getBlockCount(bitmapSize, sectorsPerCluster << sectorSizeShift),
	};

	/* Get working buffer */
	WorkBuffer workBuffer(1U << param.sectorSizeShift, 1);
	if(!workBuffer) {
		return IFS::FAT::FR_NOT_ENOUGH_CORE;
	}

	/* Create a compressed up-case table */
	uint32_t szb_case;
	uint32_t sum_case;
	LBA_t sect = dataStartSector + sectorsPerCluster * clusterLengths[0]; // Table start sector
	FRESULT fr = createUpcaseTable(device, sect, sectorSizeShift, szb_case, sum_case, workBuffer);
	if(fr) {
		return fr;
	}

	// Number of up-case table, root dir clusters
	clusterLengths[1] = getBlockCount(szb_case, sectorsPerCluster << sectorSizeShift);
	clusterLengths[2] = 1;

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
	dir[0] = EXFAT::exfat_dentry_t{EXFAT_VOLUME, .volume_label = {
													 7,
													 {'N', 'O', ' ', 'N', 'A', 'M', 'E'},
												 }};
	dir[1] = EXFAT::exfat_dentry_t{EXFAT_BITMAP, .bitmap = {
													 .start_clu = 2,
													 .size = bitmapSize,
												 }};
	dir[2] = EXFAT::exfat_dentry_t{EXFAT_UPCASE, .upcase = {
													 .checksum = sum_case,
													 .start_clu = 2 + clusterLengths[0],
													 .size = szb_case,
												 }};

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
	sect = param.volumeStartSector;
	for(unsigned n = 0; n < 2; ++n) {
		/* Main record (+0) */
		workBuffer.clear();
		auto& bpb = workBuffer.as<EXFAT::boot_sector_t>();
		bpb = EXFAT::boot_sector_t{
			.jmp_boot = {0xEB, 0x76, 0x90},
			.fs_type = FSTYPE_EXFAT,
			.partition_offset = param.volumeStartSector, // Volume offset in the physical drive [sector]
			.vol_length = param.volumeSectorCount,		 // Volume size [sector]
			.fat_offset = uint32_t(fatStartSector - param.volumeStartSector),  // FAT offset [sector]
			.fat_length = numFatSectors,									   // FAT size [sector]
			.clu_offset = uint32_t(dataStartSector - param.volumeStartSector), // Data offset [sector]
			.clu_count = numClusters,										   // Number of clusters
			.root_cluster = 2 + clusterLengths[0] + clusterLengths[1],		   // Root dir cluster #
			.vol_serial = param.volumeSerialNumber,
			.fs_revision = 0x0100, // Filesystem version (1.00)
			.sect_size_bits = sectorSizeShift,
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
		std::fill_n(sumRecord, sectorSize / sizeof(sum), sum);
		if(!WRITE_SECTORS(sumRecord, sect++, 1)) {
			return IFS::FAT::FR_DISK_ERR;
		}
	}

	return IFS::FAT::FR_OK;
}

#endif // ENABLE_EXFAT

FRESULT createFatVolume(Device& device, const FatParam& param, const bool allowFat32)
{
	auto getAllocationUnitSize = [](const uint16_t cst[], uint32_t count) -> unsigned {
		unsigned pau = 1;
		for(unsigned i = 0; cst[i] != 0 && cst[i] <= count; ++i) {
			pau <<= 1;
		}
		return pau;
	};

	auto type = param.type; // May change
	auto sectorSizeShift = param.sectorSizeShift;
	auto sectorSize = 1U << sectorSizeShift;
	auto sectorsPerCluster = param.sectorsPerCluster;

	uint32_t pau;				 // Physical Allocation Unit
	uint32_t numClusters;		 // Number of clusters
	uint32_t numFatSectors;		 // FAT size [sector]
	uint32_t numReservedSectors; // Number of reserved sectors
	uint32_t numRootDirSectors;  // Root dir size [sector]
	LBA_t fatStartSector;
	do {
		pau = sectorsPerCluster;
		/* Pre-determine number of clusters and FAT sub-type */
		if(type == DiskPart::Type::fat32) {
			if(pau == 0) {
				// Determine allocation unit size from Volume size, in unit of 128K
				static const uint16_t cst[] = {1, 2, 4, 8, 16, 32, 0};
				pau = getAllocationUnitSize(cst, param.volumeSectorCount / (128 * 1024));
			}
			numClusters = param.volumeSectorCount / pau;
			numFatSectors = getBlockCount((2 + numClusters) * sizeof(uint32_t), sectorSize);
			numReservedSectors = 32;
			numRootDirSectors = 0; // No static directory
			if(numClusters <= MAX_FAT16 || numClusters > MAX_FAT32) {
				return IFS::FAT::FR_MKFS_ABORTED;
			}
		} else {
			/* FAT volume */
			if(pau == 0) {
				// Determine allocation unit size from volume size, in units of 4K
				static const uint16_t cst[] = {1, 4, 16, 64, 256, 512};
				pau = getAllocationUnitSize(cst, param.volumeSectorCount / (4 * 1024));
			}
			numClusters = param.volumeSectorCount / pau;
			uint32_t n;
			if(numClusters > MAX_FAT12) {
				n = (2 + numClusters) * 2;
			} else {
				type = DiskPart::Type::fat12;
				n = ((2 + numClusters) * 3 + 1) / 2;
			}
			numFatSectors = getBlockCount(n, sectorSize);
			numReservedSectors = 1;
			numRootDirSectors = param.numRootEntries * sizeof(FAT::msdos_dir_entry_t) / sectorSize;
		}
		fatStartSector = param.volumeStartSector + numReservedSectors;								// FAT base sector
		LBA_t dataStartSector = fatStartSector + numFatSectors * param.numFats + numRootDirSectors; // Data base sector

		/* Align data area to erase block boundary (for flash memory media) */
		uint32_t n = align_up(dataStartSector, param.sectorsPerBlock) -
					 dataStartSector; // Sectors to next nearest from current data base
		if(type == DiskPart::Type::fat32) {
			// Move FAT
			numReservedSectors += n;
			fatStartSector += n;
		} else {
			// Expand FAT
			if(n % param.numFats != 0) {
				// Adjust fractional error
				--n;
				++numReservedSectors;
				++fatStartSector;
			}
			numFatSectors += n / param.numFats;
		}

		/* Determine number of clusters and final check of validity of the FAT sub-type */
		if(param.volumeSectorCount < dataStartSector + pau * 16 - param.volumeStartSector) {
			// Volume too small
			return IFS::FAT::FR_MKFS_ABORTED;
		}

		numClusters =
			(param.volumeSectorCount - numReservedSectors - numFatSectors * param.numFats - numRootDirSectors) / pau;
		if(type == DiskPart::Type::fat32) {
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

		if(type == DiskPart::Type::fat16) {
			if(numClusters > MAX_FAT16) {
				// Too many clusters for FAT16
				if(sectorsPerCluster == 0 && (pau * 2) <= 64) {
					// Adjust cluster size and retry
					sectorsPerCluster = pau * 2;
					continue;
				}
				if(allowFat32) {
					// Switch type to FAT32 and retry
					type = DiskPart::Type::fat32;
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

		if(type == DiskPart::Type::fat12 && numClusters > MAX_FAT12) {
			// Too many clusters for FAT12
			return IFS::FAT::FR_MKFS_ABORTED;
		}

		// Ok, cluster configuration is valid
		break;
	} while(true);

	/* Get working buffer */
	WorkBuffer workBuffer(1U << param.sectorSizeShift, 1);
	if(!workBuffer) {
		return IFS::FAT::FR_NOT_ENOUGH_CORE;
	}

#if FF_USE_TRIM
	// Inform storage device that the volume area may be erased
	device.trim(param.volumeStartSector, param.volumeSectorCount);
#endif
	/* Create FAT VBR */
	workBuffer.clear();
	auto& bpb = workBuffer.as<FAT::fat_boot_sector_t>();
	bpb = FAT::fat_boot_sector_t{
		.jmp_boot = {0xEB, 0xFE, 0x90},
		.system_id = {'M', 'S', 'D', 'O', 'S', '5', '.', '0'},
		.sector_size = uint16_t(sectorSize),
		.sec_per_clus = uint8_t(pau),
		.reserved = uint16_t(numReservedSectors),
		.num_fats = param.numFats,
		.dir_entries = uint16_t((type == DiskPart::Type::fat32) ? 0 : param.numRootEntries),
		.sectors =
			uint16_t((param.volumeSectorCount < 0x10000) ? param.volumeSectorCount : 0), // Volume size in 16-bit LBA
		.media = 0xF8,
		.fat_length = uint16_t((type != DiskPart::Type::fat32) ? numFatSectors : 0),
		.secs_track = 64,							 // Number of sectors per track (for int13)
		.heads = 255,								 // Number of heads (for int13)
		.hidden = uint32_t(param.volumeStartSector), // Volume offset in the physical drive [sector]
		.total_sect =
			uint32_t((param.volumeSectorCount > 0xffff) ? param.volumeSectorCount : 0), // Volume size in 32-bit LBA
	};
	if(type == DiskPart::Type::fat32) {
		bpb.fat32 = decltype(bpb.fat32){
			.fat_length = numFatSectors,
			.root_cluster = 2,
			.info_sector = 1,	 // Offset of FSINFO sector (VBR + 1)
			.backup_boot = 6,	 // Offset of backup VBR (VBR + 6)
			.drive_number = 0x80, // Drive number (for int13)
			.signature = 0x29,	// Extended boot signature
			.vol_id = param.volumeSerialNumber,
			.vol_label = {'N', 'O', ' ', 'N', 'A', 'M', 'E', ' ', ' ', ' ', ' '},
			.fs_type = FSTYPE_FAT32,
		};
	} else {
		bpb.fat16 = decltype(bpb.fat16){
			.drive_number = 0x80, // Drive number (for int13)
			.signature = 0x29,	// Extended boot signature
			.vol_id = param.volumeSerialNumber,
			.vol_label = {'N', 'O', ' ', 'N', 'A', 'M', 'E', ' ', ' ', ' ', ' '},
			.fs_type = FSTYPE_FAT,
		};
	}
	bpb.signature = BOOT_SIGNATURE;
	if(!WRITE_SECTORS(&bpb, param.volumeStartSector, 1)) {
		return IFS::FAT::FR_DISK_ERR;
	}

	/* Create FSINFO record if needed */
	if(type == DiskPart::Type::fat32) {
		/* Write backup VBR (VBR + 6) */
		if(!WRITE_SECTORS(&bpb, param.volumeStartSector + 6, 1)) {
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
		if(!WRITE_SECTORS(&fsinfo, param.volumeStartSector + 7, 1)) {
			return IFS::FAT::FR_DISK_ERR;
		}
		// Write original FSINFO (VBR + 1)
		if(!WRITE_SECTORS(&fsinfo, param.volumeStartSector + 1, 1)) {
			return IFS::FAT::FR_DISK_ERR;
		}
	}

	/* Initialize FAT area */
	workBuffer.clear();
	auto sect = fatStartSector;
	for(unsigned i = 0; i < param.numFats; ++i) {
		// Initialize each FAT
		if(type == DiskPart::Type::fat32) {
			auto fat = workBuffer.as<uint32_t[]>();
			fat[0] = 0xFFFFFFF8;
			fat[1] = 0xFFFFFFFF;
			fat[2] = 0x0FFFFFFF; // root directory
		} else {
			auto fat = workBuffer.as<uint16_t[]>();
			fat[0] = 0xFFF8;
			fat[1] = (type == DiskPart::Type::fat12) ? 0x00FF : 0xFFFF;
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
	for(auto nsect = (type == DiskPart::Type::fat32) ? pau : numRootDirSectors; nsect != 0;) {
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

bool calculatePartition(const MKFS_PARM& opt, DiskPart& partition, FatParam& param)
{
	/* Get physical drive status (sz_drv, sectorsPerBlock, sectorSize) */
	auto& device = *partition.device;
	uint16_t sectorSize;
#if FF_MAX_SS != FF_MIN_SS
	sectorSize = device.getSectorSize();
	param.sectorSizeShift = getSizeBits(sectorSize);
	if(sectorSize > FF_MAX_SS || sectorSize < FF_MIN_SS || sectorSize != (1U << param.sectorSizeShift)) {
		return IFS::FAT::FR_DISK_ERR;
	}
#else
	sectorSize = FF_MAX_SS;
	param.sectorSizeShift = getSizeBits(sectorSize);
#endif

	auto gptEntriesPerSector = sectorSize / sizeof(gpt_entry_t);

	param.sectorsPerBlock = opt.align ?: device.getBlockSize() >> param.sectorSizeShift;
	if(param.sectorsPerBlock == 0 || param.sectorsPerBlock > 0x8000 ||
	   (param.sectorsPerBlock & (param.sectorsPerBlock - 1))) {
		param.sectorsPerBlock = 1;
	}

	/* Options for FAT sub-type and FAT parameters */
	param.numFats = (opt.numFats >= 2) ? 2 : 1;
	param.numRootEntries = opt.numRootEntries;
	if(param.numRootEntries == 0 || param.numRootEntries > 0x8000 ||
	   (param.numRootEntries % gptEntriesPerSector) != 0) {
		param.numRootEntries = 512;
	}

	if(opt.clusterSize == 0) {
		param.sectorsPerCluster = 0; // Auto-detect later on
	} else {
		// Round up requested cluster size to nearest power of 2
		auto clusterSize = 0x80000000U >> __builtin_clz(opt.clusterSize);
		clusterSize = std::max(0x1000000U, clusterSize);
		param.sectorsPerCluster = clusterSize >> param.sectorSizeShift;
	}

	//
	param.volumeStartSector = partition.address >> param.sectorSizeShift;
	param.volumeSectorCount = partition.size >> param.sectorSizeShift;

	// Check for minimum volume size
	if(param.volumeSectorCount < 128) {
		debug_e("[DISK] Volume too small");
		return false;
	}

	/* Determine the appropriate FAT type */
	if(ENABLE_EXFAT && (opt.types == DiskPart::Type::exfat || param.volumeSectorCount >= 64 * 1024 * 1024 ||
						param.sectorsPerCluster > 128)) {
		// exFAT only, vol >= 64MS or sectorsPerCluster > 128S
		param.type = DiskPart::Type::exfat;
		partition.sysIndicator = DiskPart::SI_EXFAT;
	} else {
		if(FF_LBA64 && isSize64(param.volumeSectorCount)) {
			debug_e("[DISK] FAT32 volumes limited to 4GB");
			return false;
		}
		// Ensure AU is valid for FAT/FAT32
		param.sectorsPerCluster = std::min(param.sectorsPerCluster, 128U);
		if(opt.types == DiskPart::Type::fat32) {
			param.type = DiskPart::Type::fat32;
			partition.sysIndicator = DiskPart::SI_FAT32X;
		} else if(opt.types[DiskPart::Type::fat16]) {
			param.type = DiskPart::Type::fat16;
			partition.sysIndicator = (param.volumeSectorCount >= 0x10000) ? DiskPart::SI_FAT16B : DiskPart::SI_FAT16;
		} else {
			return false;
		}
	}

	/*
	 * https://docs.microsoft.com/en-us/windows/win32/fileio/exfat-specification#3111-volumeserialnumber-field
	 *
	 * 	 The VolumeSerialNumber field shall contain a unique serial number.
	 * 	 This assists implementations to distinguish among different exFAT volumes.
	 * 	 Implementations should generate the serial number by combining the date and time of formatting the exFAT volume.
	 * 	 The mechanism for combining date and time to form a serial number is implementation-specific.
	 */
	param.volumeSerialNumber = uint32_t(param.volumeSectorCount) ^ IFS::fsGetTimeUTC() ^ system_get_time();

	return true;
}

bool formatVolume(const DiskPart& part, FatParam& param)
{
	auto& device = *part.device;

#ifdef ENABLE_EXFAT
	if(param.type == DiskPart::Type::exfat) {
		/* Create an exFAT volume */
		createExFatVolume(device, param);

	} else
#endif
	{
		createFatVolume(device, param, opt.types[DiskPart::Type::fat32]);
	}

	return device.sync();
}

} // namespace Storage
