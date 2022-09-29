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

namespace MBR
{
struct PartitionSpec {
	storage_size_t size;
	String name;
	DiskPart::SysIndicator sysIndicator;
};

IFS::ErrorCode createPartition(Device& device, PartitionSpec* spec, size_t partitionCount);

inline IFS::ErrorCode createPartition(Device& device, PartitionSpec& spec)
{
	return createPartition(device, &spec, 1);
}

} // namespace MBR

namespace GPT
{
struct PartitionSpec {
	storage_size_t size;
	String name;
	Uuid guid;
};

IFS::ErrorCode createPartition(Device& device, const PartitionSpec* spec, size_t numSpecs);

inline IFS::ErrorCode createPartition(Device& device, PartitionSpec& spec)
{
	return createPartition(device, &spec, 1);
}

}; // namespace GPT

/*
 
Partition creation
==================

a. Use entire disk
b. Create set of partitions
 	Fail if there are existing partitions

c. Add partition
	Provide separate functions for MBR / GPT partitioning; namespaces ?
	Fail if existing partitioning type is incorrect
	Argument set will differ between partitioning types, e.g. UUID required for GPT.
	Name, UUID are optional and will be generated if not provided

d. Extend partition
	A bit much. Do this with other tools.

e. Convert between MBR / GPT
	Can be done by scanning all partitions and passing to (b)
	May fail.
	Probably best not to bother with this.

Add 'force' flag to override default (safer) behaviour

 */

/**
 * @brief Add partition to a disk
 */
// IFS::ErrorCode createPartition(DiskPart partition);

/**
 * @brief Remove partition from a disk
 * @param partition Describes exactly which partition to remove
 */
// IFS::ErrorCode removePartition(DiskPart partition);

/**
 * @brief Deduce disk partition parameters for given space
 * @param partition On success, contains description of partition to be created
 */
IFS::ErrorCode calculatePartition(const MKFS_PARM& opt, Partition partition, FatParam& param);

IFS::ErrorCode formatVolume(Partition partition, const FatParam& param);

} // namespace Storage
