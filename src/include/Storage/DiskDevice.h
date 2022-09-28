#pragma once

#include "DiskPart.h"
#include <IFS/Error.h>

namespace Storage
{
/* Format parameter structure */
struct MKFS_PARM {
	DiskPart::SysTypes types{0};  ///< Valid partition format types
	uint8_t numFats{1};			  ///< Number of FATs (1 or 2)
	unsigned align{0};			  ///< Data area alignment (sector)
	unsigned numRootEntries{512}; ///< Number of root directory entries
	uint32_t clusterSize{4096};   ///< Cluster size (byte)
};

struct FatParam {
	uint32_t sectorsPerBlock; ///< Flash erase block size
	uint32_t volumeSerialNumber;
	uint16_t numRootEntries;
	uint16_t sectorsPerCluster; ///< Set to 0 for auto-calculation
	uint8_t sectorSizeShift;
	uint8_t numFats;
	DiskPart::SysType sysType;
	//
	uint32_t numClusters;		 // Number of clusters
	uint32_t numFatSectors;		 // FAT size [sector]
	uint32_t numRootDirSectors;  // Root dir size [sector]
	uint16_t numReservedSectors; // Number of reserved sectors
	storage_size_t fatStartSector;
	DiskPart::SysIndicator sysIndicator;
};

/**
 * @brief Add partition to a disk
 */
IFS::ErrorCode createPartition(DiskPart partition);

/**
 * @brief Remove partition from a disk
 * @param partition Describes exactly which partition to remove
 */
IFS::ErrorCode removePartition(DiskPart partition);

/**
 * @brief Deduce disk partition parameters for given space
 * @param partition On success, contains description of partition to be created
 */
IFS::ErrorCode calculatePartition(const MKFS_PARM& opt, Partition partition, FatParam& param);

IFS::ErrorCode formatVolume(Partition partition, const FatParam& param);

} // namespace Storage
