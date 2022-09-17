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
	using Types = BitSet<uint8_t, DiskPart::Type>;
	static constexpr Types validTypes{Type::fat | Type::fat32 | Type::exfat};

	Type type;
	uint64_t address;
	uint64_t size;
	String name;
	Uuid guid;
	uint16_t sectorSize;  ///< Sector size (bytes)
	uint16_t clusterSize; ///< Cluster size (bytes)
	uint8_t numFat;		  ///< Number of FATs

	size_t printTo(Print& p) const;
};

} // namespace Storage

String toString(Storage::DiskPart::Type type);
