#include "include/IFS/FAT/Format.h"
#include <Storage/Disk/SectorBuffer.h>
#include <IFS/TimeStamp.h>
#include <Storage/Disk/diskdefs.h>

// Definitions from FileSystem
namespace IFS
{
namespace FAT
{
#include "../fatfs/ff.h"

using namespace Storage::Disk;
namespace Error = IFS::Error;
using ErrorCode = IFS::ErrorCode;
using Partition = Storage::Partition;
using Storage::getSizeBits;
using Storage::isSize64;

namespace
{
#ifdef ENABLE_EXFAT

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

/*
 * Create a compressed up-case table
 */
ErrorCode createUpcaseTable(Partition partition, storage_size_t sect, unsigned sectorSizeShift, uint32_t& szb_case,
							uint32_t& sum, SectorBuffer& buffer)
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
		buffer[sectorOffset++] = ch;
		sum = xsum32(ch, sum);
		buffer[sectorOffset++] = ch >> 8;
		sum = xsum32(ch >> 8, sum);
		szb_case += 2;

		// Write buffered data when buffer full or end of process
		if(si == 0 || sectorOffset == buffer.size()) {
			auto byteCount = align_up(sectorOffset, 1U << sectorSizeShift);
			if(!partition.write(sect << sectorSizeShift, buffer.get(), byteCount)) {
				return Error::WriteFailure;
			}
			sect += byteCount >> sectorSizeShift;
			sectorOffset = 0;
		}
	} while(si != 0);

	return Error::Success;
}

ErrorCode createExFatVolume(Partition partition, const FatParam& param)
{
	using namespace Storage::Disk::EXFAT;

	const auto sectorSizeShift = param.sectorSizeShift;
	const auto volumeSectorCount = partition.size() >> sectorSizeShift;
	if(volumeSectorCount < 0x1000) {
		// Volume too small for exFAT
		return Error::BadParam;
	}
	/* Determine FAT location, data location and number of clusters */
	const auto sectorSize = 1U << sectorSizeShift;
	auto sectorsPerCluster = param.sectorsPerCluster;
	if(sectorsPerCluster == 0) {
		// Auto-select cluster size
		if(volumeSectorCount >= 64 * 1024 * 1024) {
			sectorsPerCluster = 256;
		} else if(volumeSectorCount >= 512 * 1024) {
			sectorsPerCluster = 64;
		} else {
			sectorsPerCluster = 8;
		}
	}
	const uint32_t fatStartSector = 32;
	const uint32_t numFatSectors =
		align_up((volumeSectorCount / sectorsPerCluster + 2) * sizeof(uint32_t), sectorSize) >> sectorSizeShift;
	const uint32_t dataStartSector = align_up(fatStartSector + numFatSectors, param.sectorsPerBlock);
	if(dataStartSector >= volumeSectorCount / 2) {
		// Volume too small
		return Error::BadParam;
	}
	const uint32_t numClusters = (volumeSectorCount - dataStartSector) / sectorsPerCluster;
	if(numClusters < 16 || numClusters > MAX_EXFAT) {
		// Too few/many clusters
		return Error::BadParam;
	}

	const uint32_t bitmapSize = (numClusters + 7) / 8; // Size of allocation bitmap

	// Number of clusters for bitmap, upcaseTable, rootDir
	uint32_t clusterLengths[3] = {
		getBlockCount(bitmapSize, sectorsPerCluster << sectorSizeShift),
	};

	/* Get working buffer */
	SectorBuffer buffer(1U << param.sectorSizeShift, 1);
	if(!buffer) {
		return Error::NoMem;
	}

#if FF_USE_TRIM
	// Inform storage device that the volume area may be erased
	partition.trim(0, partition.size());
#endif

	auto writeSectors = [&](LBA_t sector, const void* buff, size_t count) -> bool {
		return partition.write(sector << sectorSizeShift, buff, count << sectorSizeShift);
	};

	/* Create a compressed up-case table */
	uint32_t szb_case;
	uint32_t sum_case;
	LBA_t sect = dataStartSector + sectorsPerCluster * clusterLengths[0]; // Table start sector
	auto err = createUpcaseTable(partition, sect, sectorSizeShift, szb_case, sum_case, buffer);
	if(err) {
		return err;
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
		buffer.clear();

		// Mark used clusters
		auto maxBits = 8 * buffer.size();
		for(unsigned i = 0; i < maxBits && nbit != 0; ++i, --nbit) {
			buffer[i / 8] |= 1 << (i % 8);
		}
		auto n = std::min(nsect, buffer.sectors());
		if(!writeSectors(sect, buffer.get(), n)) {
			return Error::WriteFailure;
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
		buffer.clear();
		auto fat = buffer.as<uint32_t[]>();
		auto clusterCount = buffer.size() / sizeof(uint32_t);
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
		auto n = std::min(nsect, buffer.sectors());
		if(!writeSectors(sect, fat, n)) {
			return Error::WriteFailure;
		}
		sect += n;
		nsect -= n;
	} while(nsect != 0);

	/* Initialize the root directory */
	buffer.clear();
	auto dir = buffer.as<EXFAT::exfat_dentry_t[]>();
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
		auto n = std::min(nsect, buffer.sectors());
		if(!writeSectors(sect, dir, n)) {
			return Error::WriteFailure;
		}
		// Rest of entries are filled with zero
		buffer.clear();
		sect += n;
		nsect -= n;
	} while(nsect != 0);

	/* Create two sets of the exFAT VBR blocks */
	sect = 0;
	for(unsigned n = 0; n < 2; ++n) {
		/* Main record (+0) */
		buffer.clear();
		auto& bpb = buffer.as<EXFAT::boot_sector_t>();
		bpb = EXFAT::boot_sector_t{
			.jmp_boot = {0xEB, 0x76, 0x90},
			.fs_type = FSTYPE_EXFAT,
			.partition_offset = partition.address() >> sectorSizeShift, // Volume offset in the physical drive [sector]
			.vol_length = volumeSectorCount,							// Volume size [sector]
			.fat_offset = fatStartSector,								// FAT offset [sector]
			.fat_length = numFatSectors,								// FAT size [sector]
			.clu_offset = uint32_t(dataStartSector),					// Data offset [sector]
			.clu_count = numClusters,									// Number of clusters
			.root_cluster = 2 + clusterLengths[0] + clusterLengths[1],  // Root dir cluster #
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
			sum = xsum32(buffer[i], sum);
		}
		if(!writeSectors(sect++, &bpb, 1)) {
			return Error::WriteFailure;
		}
		/* Extended bootstrap record (+1..+8) */
		buffer.clear();
		bpb.signature = BOOT_SIGNATURE;
		unsigned sectorIndex = 1;
		for(; sectorIndex < 9; ++sectorIndex) {
			sum = xsum32(&bpb, sectorSize, sum);
			if(!writeSectors(sect++, &bpb, 1)) {
				return Error::WriteFailure;
			}
		}
		/* OEM/Reserved record (+9..+10) */
		buffer.clear();
		for(; sectorIndex < 11; sectorIndex++) {
			sum = xsum32(buffer.get(), sectorSize, sum);
			if(!writeSectors(sect++, buffer.get(), 1)) {
				return Error::WriteFailure;
			}
		}
		/* Fill sum record (+11) with checksum value */
		auto sumRecord = buffer.as<uint32_t[]>();
		std::fill_n(sumRecord, sectorSize / sizeof(sum), sum);
		if(!writeSectors(sect++, sumRecord, 1)) {
			return Error::WriteFailure;
		}
	}

	return Error::Success;
}

#endif // ENABLE_EXFAT

ErrorCode calculateFatParam(FatParam& param, const uint32_t volumeSectorCount, const bool allowFat32)
{
	using namespace Storage::Disk::FAT;

	auto getAllocationUnitSize = [](const uint16_t cst[], uint32_t count) -> unsigned {
		unsigned pau = 1;
		for(unsigned i = 0; cst[i] != 0 && cst[i] <= count; ++i) {
			pau <<= 1;
		}
		return pau;
	};

	auto type = param.sysType; // May change
	const auto sectorSizeShift = param.sectorSizeShift;
	const auto sectorSize = 1U << sectorSizeShift;
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
		if(type == SysType::fat32) {
			if(pau == 0) {
				// Determine allocation unit size from Volume size, in unit of 128K
				static const uint16_t cst[] = {1, 2, 4, 8, 16, 32, 0};
				pau = getAllocationUnitSize(cst, volumeSectorCount / (128 * 1024));
			}
			numClusters = volumeSectorCount / pau;
			numFatSectors = getBlockCount((2 + numClusters) * sizeof(uint32_t), sectorSize);
			numReservedSectors = 32;
			numRootDirSectors = 0; // No static directory
			if(numClusters <= MAX_FAT16 || numClusters > MAX_FAT32) {
				return Error::BadParam;
			}
		} else {
			/* FAT volume */
			if(pau == 0) {
				// Determine allocation unit size from volume size, in units of 4K
				static const uint16_t cst[] = {1, 4, 16, 64, 256, 512};
				pau = getAllocationUnitSize(cst, volumeSectorCount / (4 * 1024));
			}
			numClusters = volumeSectorCount / pau;
			uint32_t n;
			if(numClusters > MAX_FAT12) {
				n = (2 + numClusters) * 2;
			} else {
				type = SysType::fat12;
				n = ((2 + numClusters) * 3 + 1) / 2;
			}
			numFatSectors = getBlockCount(n, sectorSize);
			numReservedSectors = 1;

			numRootDirSectors = param.numRootEntries * sizeof(msdos_dir_entry_t) / sectorSize;
		}
		fatStartSector = numReservedSectors;														// FAT base sector
		LBA_t dataStartSector = fatStartSector + numFatSectors * param.numFats + numRootDirSectors; // Data base sector

		/* Align data area to erase block boundary (for flash memory media) */
		uint32_t n = align_up(dataStartSector, param.sectorsPerBlock) -
					 dataStartSector; // Sectors to next nearest from current data base
		if(type == SysType::fat32) {
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
		if(volumeSectorCount < dataStartSector + pau * 16) {
			// Volume too small
			return Error::BadParam;
		}

		numClusters =
			(volumeSectorCount - numReservedSectors - numFatSectors * param.numFats - numRootDirSectors) / pau;
		if(type == SysType::fat32) {
			if(numClusters <= MAX_FAT16) {
				// Too few clusters for FAT32
				if(sectorsPerCluster == 0) {
					// Adjust cluster size and retry
					sectorsPerCluster = pau / 2;
					if(sectorsPerCluster != 0) {
						continue;
					}
				}
				return Error::BadParam;
			}
		}

		if(type == SysType::fat16) {
			if(numClusters > MAX_FAT16) {
				// Too many clusters for FAT16
				if(sectorsPerCluster == 0 && (pau * 2) <= 64) {
					// Adjust cluster size and retry
					sectorsPerCluster = pau * 2;
					continue;
				}
				if(allowFat32) {
					// Switch type to FAT32 and retry
					type = SysType::fat32;
					continue;
				}
				if(sectorsPerCluster == 0) {
					// Adjust cluster size and retry
					sectorsPerCluster = pau * 2;
					if(sectorsPerCluster <= 128) {
						continue;
					}
				}
				return Error::BadParam;
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
				return Error::BadParam;
			}
		}

		if(type == SysType::fat12 && numClusters > MAX_FAT12) {
			// Too many clusters for FAT12
			return Error::BadParam;
		}

		// Ok, cluster configuration is valid
		break;
	} while(true);

	param.sectorsPerCluster = pau;
	param.sysType = type;

	param.numClusters = numClusters;
	param.numFatSectors = numFatSectors;
	param.numReservedSectors = numReservedSectors;
	param.numRootDirSectors = numRootDirSectors;
	param.fatStartSector = fatStartSector;

	return Error::Success;
}

ErrorCode createFatVolume(Partition partition, const FatParam& param)
{
	using namespace Storage::Disk::FAT;

	const auto sectorSizeShift = param.sectorSizeShift;
	const uint16_t sectorSize = 1U << sectorSizeShift;

	/* Get working buffer */
	SectorBuffer buffer(sectorSize, 1);
	if(!buffer) {
		return Error::NoMem;
	}

#if FF_USE_TRIM
	// Inform storage device that the volume area may be erased
	partition.trim(0, partition.size());
#endif

	auto writeSectors = [&](storage_size_t sector, const void* buff, size_t count) -> bool {
		return partition.write(sector << sectorSizeShift, buff, count << sectorSizeShift);
	};

	/* Create FAT VBR */
	const uint32_t volumeSectorCount = partition.size() >> sectorSizeShift;
	buffer.clear();
	auto& bpb = buffer.as<fat_boot_sector_t>();
	bpb = fat_boot_sector_t{
		.jmp_boot = {0xEB, 0xFE, 0x90},
		.system_id = {'M', 'S', 'D', 'O', 'S', '5', '.', '0'},
		.sector_size = sectorSize,
		.sec_per_clus = uint8_t(param.sectorsPerCluster),
		.reserved = param.numReservedSectors,
		.num_fats = param.numFats,
		.dir_entries = (param.sysType == SysType::fat32) ? uint16_t(0) : param.numRootEntries,
		.sectors = uint16_t((volumeSectorCount < 0x10000) ? volumeSectorCount : 0), // Volume size in 16-bit LBA
		.media = 0xF8,
		.fat_length = uint16_t((param.sysType != SysType::fat32) ? param.numFatSectors : 0),
		.secs_track = 64,											// Number of sectors per track (for int13)
		.heads = 255,												// Number of heads (for int13)
		.hidden = uint32_t(partition.address() >> sectorSizeShift), // Volume offset in the physical drive [sector]
		.total_sect = uint32_t((volumeSectorCount > 0xffff) ? volumeSectorCount : 0), // Volume size in 32-bit LBA
	};
	if(param.sysType == SysType::fat32) {
		bpb.fat32 = decltype(bpb.fat32){
			.fat_length = param.numFatSectors,
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
	if(!writeSectors(0, &bpb, 1)) {
		return Error::WriteFailure;
	}

	/* Create FSINFO record if needed */
	if(param.sysType == SysType::fat32) {
		/* Write backup VBR (VBR + 6) */
		if(!writeSectors(6, &bpb, 1)) {
			return Error::WriteFailure;
		}
		buffer.clear();
		auto& fsinfo = buffer.as<fat_boot_fsinfo_t>();
		fsinfo = fat_boot_fsinfo_t{
			.signature1 = FAT_FSINFO_SIG1,
			.signature2 = FAT_FSINFO_SIG2,
			.free_clusters = param.numClusters - 1,
			.next_cluster = 2,
			.signature = BOOT_SIGNATURE,
		};
		// Write backup FSINFO (VBR + 7)
		if(!writeSectors(7, &fsinfo, 1)) {
			return Error::WriteFailure;
		}
		// Write original FSINFO (VBR + 1)
		if(!writeSectors(1, &fsinfo, 1)) {
			return Error::WriteFailure;
		}
	}

	/* Initialize FAT area */
	buffer.clear();
	auto sect = param.fatStartSector;
	for(unsigned i = 0; i < param.numFats; ++i) {
		// Initialize each FAT
		if(param.sysType == SysType::fat32) {
			auto fat = buffer.as<uint32_t[]>();
			fat[0] = 0xFFFFFFF8;
			fat[1] = 0xFFFFFFFF;
			fat[2] = 0x0FFFFFFF; // root directory
		} else {
			auto fat = buffer.as<uint16_t[]>();
			fat[0] = 0xFFF8;
			fat[1] = (param.sysType == SysType::fat12) ? 0x00FF : 0xFFFF;
		}
		for(auto nsect = param.numFatSectors; nsect != 0;) {
			// Fill FAT sectors
			auto n = std::min(nsect, buffer.sectors());
			if(!writeSectors(sect, buffer.get(), n)) {
				return Error::WriteFailure;
			}
			// Rest of FAT is empty
			buffer.clear();
			sect += n;
			nsect -= n;
		}
	}

	/* Initialize root directory (fill with zero) */
	for(auto nsect = (param.sysType == SysType::fat32) ? param.sectorsPerCluster : param.numRootDirSectors;
		nsect != 0;) {
		auto n = std::min(nsect, buffer.sectors());
		if(!writeSectors(sect, buffer.get(), n)) {
			return Error::WriteFailure;
		}
		sect += n;
		nsect -= n;
	}

	return Error::Success;
}

} // namespace

ErrorCode calculatePartition(Partition partition, const MKFS_PARM& opt, FatParam& param)
{
	param = FatParam{};

	if(opt.types - fatTypes) {
		debug_e("[FAT] calculatePartition invalid types");
		return Error::BadParam;
	}

	/* Get physical drive status (sz_drv, sectorsPerBlock, sectorSize) */
	uint16_t sectorSize;
#if FF_MAX_SS != FF_MIN_SS
	sectorSize = partition.getSectorSize();
	if(sectorSize > FF_MAX_SS || sectorSize < FF_MIN_SS || !isLog2(sectorSize)) {
		return Error::BadParam;
	}
#else
	sectorSize = FF_MAX_SS;
#endif
	param.sectorSizeShift = getSizeBits(sectorSize);

	auto gptEntriesPerSector = sectorSize / sizeof(gpt_entry_t);

	param.sectorsPerBlock = opt.align ?: partition.getBlockSize() >> param.sectorSizeShift;
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
		clusterSize = std::min(0x1000000U, clusterSize);
		param.sectorsPerCluster = clusterSize >> param.sectorSizeShift;
	}

	//

	const auto volumeSectorCount = partition.size() >> param.sectorSizeShift;

	// Check for minimum volume size
	if(volumeSectorCount < 128) {
		debug_e("[FAT] Volume too small");
		return Error::BadParam;
	}

/* Determine the appropriate FAT type */
#ifdef ENABLE_EXFAT
	if(opt.types == SysType::exfat || volumeSectorCount >= 64 * 1024 * 1024 || param.sectorsPerCluster > 128) {
		// exFAT only, vol >= 64MS or sectorsPerCluster > 128S
		param.sysType = SysType::exfat;
		param.sysIndicator = SI_EXFAT;
	} else
#endif
	{
#ifdef ENABLE_STORAGE_SIZE64
		if(isSize64(volumeSectorCount)) {
			debug_e("[FAT] FAT32 volumes limited to 4GB");
			return Error::BadParam;
		}
#endif
		// Ensure AU is valid for FAT/FAT32
		param.sectorsPerCluster = std::min(param.sectorsPerCluster, uint16_t(128));
		if(opt.types == SysType::fat32) {
			param.sysType = SysType::fat32;
		} else if(opt.types.none() || opt.types[SysType::fat16]) {
			param.sysType = SysType::fat16;
		} else {
			debug_e("[FAT] Volume too large");
			return Error::BadParam;
		}

		auto err = calculateFatParam(param, volumeSectorCount, opt.types.none() || opt.types[SysType::fat32]);
		if(err) {
			debug_e("[FAT] FAT parameter calculation failed");
			return err;
		}

		/* Determine system ID in the MBR partition table */
		if(param.sysType == SysType::fat32) {
			// FAT32X
			param.sysIndicator = SI_FAT32X;
		} else if(volumeSectorCount >= 0x10000) {
			// FAT12/16 (large)
			param.sysIndicator = SI_FAT16B;
		} else if(param.sysType == SysType::fat16) {
			param.sysIndicator = SI_FAT16;
		} else {
			param.sysIndicator = SI_FAT12;
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
	param.volumeSerialNumber = uint32_t(volumeSectorCount) ^ IFS::fsGetTimeUTC() ^ system_get_time();

	return Error::Success;
}

ErrorCode formatVolume(Partition partition, const FatParam& param)
{
	ErrorCode err;
#ifdef ENABLE_EXFAT
	if(param.sysType == SysType::exfat) {
		/* Create an exFAT volume */
		err = createExFatVolume(partition, param);

	} else
#endif
	{
		err = createFatVolume(partition, param);
	}

	return err ?: partition.sync() ? Error::Success : Error::WriteFailure;
}

} // namespace FAT
} // namespace IFS
