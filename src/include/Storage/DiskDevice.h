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
	uint32_t sectorsPerCluster; ///< Set to 0 for auto-calculation
	uint32_t sectorsPerBlock;   ///< Flash erase block size
	uint32_t volumeSerialNumber;
	DiskPart::Type type;
	uint16_t numRootEntries;
	uint8_t numFats;
};

/**
 * @brief Add partition to a disk
 */
bool createPartition(Partition partition);

/**
 * @brief Remove partition from a disk
 * @param partition Describes exactly which partition to remove
 */
bool removePartition(Partition partition);

int f_mkfs(Device& device, Storage::MKFS_PARM opt);

} // namespace Storage
