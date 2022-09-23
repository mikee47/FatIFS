#pragma once

#include <Storage/Device.h>
#include "DiskPart.h"
#include "WorkBuffer.h"

namespace Storage
{
namespace diskdefs
{
struct gpt_mbr_record_t;
};

class DiskScanner
{
public:
	DiskScanner(Device& device);
	~DiskScanner();

	void rewind()
	{
		partitionIndex = 0;
	}

	std::unique_ptr<DiskPart::Info> next();

private:
	enum class State {
		idle,
		MBR, ///< Master Boot Record
		GPT, ///< GUID Partition Table
		error,
		done,
	};

	unsigned scanMbrEntries(uint32_t baseLba);

	Device& device;
	diskdefs::WorkBuffer buffer;
	diskdefs::WorkBuffer entryBuffer; // GPT
	State state{};
	uint64_t sector{0};										  // GPT
	std::unique_ptr<diskdefs::gpt_mbr_record_t[]> mbrEntries; // MBR
	uint16_t numPartitionEntries{0};
	uint16_t partitionIndex{0};
	uint16_t sectorSize{0};
	uint8_t sectorSizeShift{0};
};

bool scanDiskPartitions(Device& device);

} // namespace Storage
