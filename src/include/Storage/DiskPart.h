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

	/**
	 * @brief MBR partition system type indicator values
	 * @see https://en.wikipedia.org/wiki/Partition_type#List_of_partition_IDs
	 */
	enum SysIndicator {
		SI_EXFAT = 0x07,
		SI_FAT32X = 0x0c, ///< FAT32 with LBA
		SI_FAT16B = 0x06, ///< FAT16B with 65536 or more sectors
		SI_FAT16 = 0x04,  ///< FAT16 with fewer than 65536 sectors
		SI_FAT12 = 0x01,
	};

	Type type;
	uint64_t address;
	uint64_t size;
	String name;
	Uuid guid;
	uint16_t sectorSize;  ///< Sector size (bytes)
	uint16_t clusterSize; ///< Cluster size (bytes)
	uint8_t numFat;		  ///< Number of FATs
	SysIndicator sys_ind; ///< Partition sys value

	bool isFat() const
	{
		return (Type::fat12 | Type::fat16 | Type::fat32 | Type::exfat)[type];
	}

	size_t printTo(Print& p) const;
};

} // namespace Storage

String toString(Storage::DiskPart::Type type);
