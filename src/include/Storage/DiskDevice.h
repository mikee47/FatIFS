#pragma once

#include "DiskPart.h"

namespace Storage
{
/* Format parameter structure */
struct MKFS_PARM {
	DiskPart::Types types;   ///< Valid partition format types
	bool createPartition;	///< true to create MBR/GPT, false to use whole disk
	uint8_t numFats;		 ///< Number of FATs (1 or 2)
	unsigned align;			 ///< Data area alignment (sector)
	unsigned numRootEntries; ///< Number of root directory entries
	uint32_t clusterSize;	///< Cluster size (byte)
};

struct FatParam {
	storage_size_t volumeStartSector;
	storage_size_t volumeSectorCount;
	uint32_t sectorsPerBlock; ///< Flash erase block size
	uint32_t volumeSerialNumber;
	uint16_t numRootEntries;
	uint16_t sectorsPerCluster; ///< Set to 0 for auto-calculation
	uint8_t sectorSizeShift;
	uint8_t numFats;
	DiskPart::Type type;
	//
	uint32_t numClusters;		 // Number of clusters
	uint32_t numFatSectors;		 // FAT size [sector]
	uint32_t numRootDirSectors;  // Root dir size [sector]
	uint16_t numReservedSectors; // Number of reserved sectors
	storage_size_t fatStartSector;
};

/**
 * @brief Add partition to a disk
 */
bool createPartition(Device& device, const DiskPart& partition);

/**
 * @brief Remove partition from a disk
 * @param partition Describes exactly which partition to remove
 */
bool removePartition(Device& device, const DiskPart& partition);

/**
 * @brief Deduce disk partition parameters for given space
 * @param partition On success, contains description of partition to be created
 */
bool calculatePartition(const MKFS_PARM& opt, DiskPart& partition, FatParam& param);

bool formatVolume(const DiskPart& part, FatParam& param);

} // namespace Storage
