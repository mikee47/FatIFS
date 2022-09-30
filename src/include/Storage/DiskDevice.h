#pragma once

#include "DiskPart.h"
#include <IFS/Error.h>

namespace Storage
{
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


} // namespace Storage
