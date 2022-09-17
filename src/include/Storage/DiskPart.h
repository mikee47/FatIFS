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
		fat12,
		fat16,
		fat32,
		exfat,
	};
	using Types = BitSet<uint8_t, DiskPart::Type>;

	Type type;
	uint64_t address;
	uint64_t size;
	String name;
	Uuid guid;
	uint16_t sectorSize;  ///< Sector size (bytes)
	uint16_t clusterSize; ///< Cluster size (bytes)
	uint8_t numFat;		  ///< Number of FATs

	bool isFat() const
	{
		return (Type::fat12 | Type::fat16 | Type::fat32 | Type::exfat)[type];
	}

	size_t printTo(Print& p) const;
};

} // namespace Storage

String toString(Storage::DiskPart::Type type);
