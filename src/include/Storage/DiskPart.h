#pragma once

#include <Storage/Device.h>
#include <Data/Uuid.h>
#include <Printable.h>

namespace Storage
{
class DiskPart : public Partition
{
public:
	enum class SysType : uint8_t {
		invalid, ///< MBR invalid
		unknown, ///< MBR valid but partition type not recognised
		fat12,
		fat16,
		fat32,
		exfat,
	};
	using SysTypes = BitSet<uint8_t, SysType>;

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

	struct Info : public Partition::Info {
		Uuid guid;
		uint32_t clusterSize{}; ///< Cluster size (bytes)
		uint16_t sectorSize{};  ///< Sector size (bytes)
		SysType systype{};
		SysIndicator sysind{}; ///< Partition sys value

		template <typename... Args> Info(Args... args) : Partition::Info(args...)
		{
			infosize = sizeof(Info);
		}

		bool isFat() const
		{
			return (SysType::fat12 | SysType::fat16 | SysType::fat32 | SysType::exfat)[systype];
		}
	};

	using Partition::Partition;

	DiskPart(Partition other) : Partition(other)
	{
	}

	size_t printTo(Print& p) const;
};

} // namespace Storage

String toString(Storage::DiskPart::SysType type);
