#pragma once

#include "DiskPart.h"

namespace Storage
{
/* Format options (2nd argument of f_mkfs) */
#define FM_FAT 0x01
#define FM_FAT32 0x02
#define FM_EXFAT 0x04
#define FM_ANY 0x07
#define FM_SFD 0x08

/* Format parameter structure */
struct MKFS_PARM {
	/* Format options (2nd argument of f_mkfs) */
	BitSet<uint8_t, DiskPart::Type> type;
	uint8_t fmt;
	uint8_t n_fat;	///< Number of FATs
	unsigned align;   ///< Data area alignment (sector)
	unsigned n_root;  ///< Number of root directory entries
	uint32_t au_size; ///< Cluster size (byte)
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
