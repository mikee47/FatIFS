#pragma once

#include <Storage/Device.h>
#include <Data/Uuid.h>
#include "WorkBuffer.h"

namespace Storage
{
struct DiskPartition {
	enum class Type {
		unknown,
		invalid,
		fat,
		fat32,
		exfat,
	};

	Type type;
	uint64_t address;
	uint64_t size;
	String name;
	Uuid guid;
};

class DiskScanner
{
public:
	DiskScanner(Device& device) : device(device)
	{
	}

	void rewind()
	{
		partitionIndex = 0;
	}

	bool next(DiskPartition& part);

private:
	enum class State {
		idle,
		MBR, ///< Master Boot Record
		GPT, ///< GUID Partition Table
		error,
		done,
	};

	Device& device;
	diskdefs::WorkBuffer buffer;
	diskdefs::WorkBuffer entryBuffer; // GPT
	uint16_t sectorSize{0};
	uint16_t numPartitionEntries{0};
	uint16_t partitionIndex{0};
	State state{};
	uint64_t sector{0};  // GPT
	uint32_t partlba[4]; // MBR
};

bool scanDiskPartitions(Device& device);

} // namespace Storage

String toString(Storage::DiskPartition::Type type);
