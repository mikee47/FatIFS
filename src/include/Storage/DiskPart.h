#pragma once

#include <Storage/Device.h>
#include <Data/Uuid.h>
#include <Printable.h>

namespace Storage
{
struct DiskPart {
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
	uint8_t numFat;		  ///< Number of FATs
	uint16_t sectorSize;  ///< Sector size (bytes)
	uint16_t clusterSize; ///< Cluster size (bytes)

	size_t printTo(Print& p) const;
};

} // namespace Storage

String toString(Storage::DiskPart::Type type);
