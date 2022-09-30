#pragma once

#include <Storage/DiskPart.h>
#include <IFS/Error.h>

namespace IFS
{
namespace FAT
{
using DiskPart = Storage::DiskPart;

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
 * @brief Deduce disk partition parameters for given space
 * @param partition The partition to format
 * @param opt Formatting options
 * @param param On success, contains calculated parameters for FAT volume
 * @retval IFS::ErrorCode
 *
 * When partitioning using MBR format, this method can be used to determine the `Sys indicator` value setting.
 */
IFS::ErrorCode calculatePartition(Storage::Partition partition, const MKFS_PARM& opt, FatParam& param);

/**
 * @brief Format disk partition using pre-calculated FAT parameters
 * @param partition The partition to format
 * @param param Detailed FAT parameters (returned from `calculatePartition`)
 * @retval IFS::ErrorCode
 *
 * This function allows fine control over exactly how a FAT partition is constructed.
 * Generally the `calculatePartition` function should be used to populate the `param` structure,
 * then any modifications can be made as required before actually formatting the volume.
 */
IFS::ErrorCode formatVolume(Storage::Partition partition, const FatParam& param);

/**
 * @brief Format disk partition
 * @param partition The partition to format
 * @param opt Formatting options
 * @retval IFS::ErrorCode
 */
inline IFS::ErrorCode formatVolume(Storage::Partition partition, const MKFS_PARM& opt = {})
{
	FatParam param;
	auto err = calculatePartition(partition, opt, param);
	return err ?: formatVolume(partition, param);
}

} // namespace FAT
} // namespace IFS
