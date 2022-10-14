#pragma once

#include <Storage/Disk/PartInfo.h>
#include <IFS/Error.h>

namespace IFS
{
namespace FAT
{
using namespace Storage::Disk;
using Storage::Partition;

/**
 * @brief Formatting options
 */
struct FormatOptions {
	String volumeLabel;
	SysTypes types{0};			  ///< Valid partition format types
	uint8_t numFats{1};			  ///< Number of FATs (1 or 2)
	unsigned align{0};			  ///< Data area alignment (sector)
	unsigned numRootEntries{512}; ///< Number of root directory entries
	uint32_t clusterSize{4096};   ///< Cluster size (byte)
};

struct FatParam {
	String volumeLabel;
	uint32_t sectorsPerBlock; ///< Flash erase block size
	uint32_t volumeSerialNumber;
	uint16_t numRootEntries;
	uint16_t sectorsPerCluster; ///< Set to 0 for auto-calculation
	uint8_t sectorSizeShift;
	uint8_t numFats;
	SysType sysType;
	//
	uint32_t numClusters;		 // Number of clusters
	uint32_t numFatSectors;		 // FAT size [sector]
	uint32_t numRootDirSectors;  // Root dir size [sector]
	uint16_t numReservedSectors; // Number of reserved sectors
	storage_size_t fatStartSector;
	SysIndicator sysIndicator;
};

/**
 * @brief Deduce FAT volume parameters for given space
 * @param partition The partition to format
 * @param opt Formatting options
 * @param param On success, contains calculated parameters for FAT volume
 * @retval ErrorCode
 *
 * When partitioning using MBR format, this method can be used to determine the `Sys indicator` value setting.
 */
ErrorCode calculateFatParam(Partition partition, const FormatOptions& opt, FatParam& param);

/**
 * @brief Format partition using pre-calculated FAT parameters
 * @param partition The partition to format
 * @param param Detailed FAT parameters (returned from `calculateFatParam`)
 * @retval ErrorCode
 *
 * This function allows fine control over exactly how a FAT partition is constructed.
 * Generally the `calculateFatParam` function should be used to populate the `param` structure,
 * then any modifications can be made as required before actually formatting the volume.
 */
ErrorCode formatVolume(Partition partition, const FatParam& param);

/**
 * @brief Format partition with a blank FAT volume
 * @param partition The partition to format
 * @param opt Formatting options
 * @retval ErrorCode
 */
inline ErrorCode formatVolume(Partition partition, const FormatOptions& opt = {})
{
	FatParam param;
	return calculateFatParam(partition, opt, param) ?: formatVolume(partition, param);
}

} // namespace FAT
} // namespace IFS
