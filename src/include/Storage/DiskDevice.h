#pragma once

#include "DiskPart.h"

namespace Storage
{
/* Format parameter structure */
struct MKFS_PARM {
	DiskPart::Types types; ///< Valid partition format types
	bool createPartition;  ///< true to create MBR/GPT, false to use whole disk
	uint8_t n_fat;		   ///< Number of FATs
	unsigned align;		   ///< Data area alignment (sector)
	unsigned n_root;	   ///< Number of root directory entries
	uint32_t au_size;	  ///< Cluster size (byte)
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

} // namespace Storage
