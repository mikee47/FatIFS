#pragma once

#include <Storage/Device.h>

namespace Storage
{
class DiskScanner
{
public:
	DiskScanner(Device& device) : device(device)
	{
	}

private:
	Device& device;
};

bool scanDiskPartitions(Device& device);

} // namespace Storage
