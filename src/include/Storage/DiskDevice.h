#pragma once

#include "DiskPart.h"
#include <IFS/Error.h>

namespace Storage
{
struct BasePartitionSpec {
	/**
	 * @brief Size of volume in bytes, or as percentage of device size
	 * Volume size will be rounded down to the appropriate alignment for the partitioning scheme.
	 */
	storage_size_t size;
	/**
	 * @brief Partition name. Optional.
	 */
	String name;
};

namespace MBR
{
/**
 * @brief Specification for creating a partition using the MBR scheme
 */
struct PartitionSpec : BasePartitionSpec {
	/**
	 * @brief Partition identifier
	 */
	DiskPart::SysIndicator sysIndicator;
};

/**
 * @brief Re-partition a device with the given set of partitions using the MBR scheme
 * @param device
 * @param spec List of partition specifications
 * @param numSpecs Number of partitions to create
 * @retval IFS::ErrorCode On success, number of partitions created
 * @note All existing partition information is destroyed
 *
 * Returned number of partitions may be fewer than requested if there was insufficient space.
 */
IFS::ErrorCode createPartition(Device& device, PartitionSpec* spec, size_t partitionCount);

/**
 * @brief Create a single MBR partition
 * @note All existing partition information is destroyed
 */
inline IFS::ErrorCode createPartition(Device& device, PartitionSpec& spec)
{
	return createPartition(device, &spec, 1);
}

} // namespace MBR

namespace GPT
{
/**
 * @brief Specification for creating a partition using the GPT scheme
 */
struct PartitionSpec : BasePartitionSpec {
	/**
	 * @brief Random unique GUID to use
	 * 
	 * If null (default) then GUID will be generated automatically.
	 */
	Uuid uniqueGuid;
};

/**
 * @brief Re-partition a device with the given set of GPT BASIC partitions
 * @param device
 * @param spec List of partition specifications
 * @param numSpecs Number of partitions to create
 * @retval IFS::ErrorCode On success, number of partitions created
 * @note All existing partition information is destroyed
 *
 * Returned number of partitions may be fewer than requested if there was insufficient space.
 */
IFS::ErrorCode createPartition(Device& device, const PartitionSpec* spec, size_t numSpecs);

/**
 * @brief Create a single GPT BASIC partition
 * @note All existing partition information is destroyed
 */
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

} // namespace Storage
