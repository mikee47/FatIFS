#include "include/Storage/DiskDevice.h"
#include "include/Storage/WorkBuffer.h"
#include <Storage/CustomDevice.h>
#include <IFS/TimeStamp.h>
#include <IFS/Error.h>
#include <debug_progmem.h>
#include "diskdefs.h"

extern "C" {
uint32_t os_random();
int os_get_random(void* buf, size_t len);
}

// Definitions from FileSystem
namespace IFS
{
namespace FAT
{
#include "../fatfs/ff.h"

extern uint32_t tchar2uni(const TCHAR** str);

} // namespace FAT
} // namespace IFS

namespace Storage
{
using namespace diskdefs;
namespace Error = IFS::Error;
using ErrorCode = IFS::ErrorCode;

using LBA_t = IFS::FAT::LBA_t;

/* Find an FAT volume */
/* (It supports only generic partitioning rules, MBR, GPT and SFD) */

/*-----------------------------------------------------------------------*/
/* Create FAT/exFAT volume (with sub-functions)                          */
/*-----------------------------------------------------------------------*/

#if FF_LBA64
namespace GPT
{
/* Create partitions in GPT format */
ErrorCode createPartition(Device& device, const PartitionSpec* partitionSpec, size_t numSpecs)
{
	if(partitionSpec == nullptr || numSpecs == 0) {
		return Error::BadParam;
	}

	uint16_t sectorSize;
#if FF_MAX_SS != FF_MIN_SS
	sectorSize = device.getSectorSize();
	if(sectorSize > FF_MAX_SS || sectorSize < FF_MIN_SS || !isLog2(sectorSize)) {
		return Error::BadParam;
	}
#else
	sectorSize = FF_MAX_SS;
#endif
	uint8_t sectorSizeShift = getSizeBits(sectorSize);

	/* Get working buffer */
	WorkBuffer workBuffer(sectorSize, 1);
	if(!workBuffer) {
		return Error::NoMem;
	}

	auto writeSectors = [&](LBA_t sector, const void* buff, size_t count) -> bool {
		return device.write(sector << sectorSizeShift, buff, count << sectorSizeShift);
	};

	auto driveSectors = device.getSectorCount();
	auto partAlignSectors = GPT_ALIGN >> sectorSizeShift; // Partition alignment for GPT [sector]
	auto numPartitionTableSectors =
		GPT_ITEMS * sizeof(gpt_entry_t) >> sectorSizeShift; // Size of partition table [sector]
	uint64_t backupPartitionTableSector = driveSectors - numPartitionTableSectors - 1;
	uint64_t nextAllocatableSector = 2 + numPartitionTableSectors;
	uint64_t sz_pool = backupPartitionTableSector - nextAllocatableSector; // Size of allocatable area
	uint32_t bcc = 0;													   // Cumulative partition entry checksum
	uint64_t sz_part = 1;
	unsigned partitionIndex = 0; // partition table index
	auto entries = workBuffer.as<gpt_entry_t[]>();
	auto entriesPerSector = sectorSize / sizeof(gpt_entry_t);
	for(; partitionIndex < GPT_ITEMS; ++partitionIndex) {
		auto i = partitionIndex % entriesPerSector;
		if(i == 0) {
			workBuffer.clear();
		}

		if(partitionIndex >= numSpecs) {
			partitionSpec = nullptr;
		}

		// Is the size table not terminated?
		if(partitionSpec != nullptr) {
			// Align partition start
			nextAllocatableSector = align_up(nextAllocatableSector, partAlignSectors);
			// Is the size in percentage?
			if(partitionSpec->size <= 100) {
				sz_part = sz_pool * partitionSpec->size / 100;
				// Align partition end
				sz_part = align_up(sz_part, partAlignSectors);
			} else {
				sz_part = partitionSpec->size >> sectorSizeShift;
			}
			// Clip the size at end of the pool
			if(nextAllocatableSector + sz_part > backupPartitionTableSector) {
				if(nextAllocatableSector < backupPartitionTableSector) {
					sz_part = backupPartitionTableSector - nextAllocatableSector;
				} else {
					// No room for any more partitions
					// TODO: How to report this to the user? Return partition count?
					sz_part = 0;
					partitionSpec = nullptr;
				}
			}
		}

		// Add a partition?
		if(partitionSpec != nullptr) {
			auto& entry = entries[i];
			entry.partition_type_guid = PARTITION_BASIC_DATA_GUID;
			if(partitionSpec->guid) {
				entry.unique_partition_guid = partitionSpec->guid;
			} else {
				os_get_random(&entry.unique_partition_guid, sizeof(efi_guid_t));
			}
			entry.starting_lba = nextAllocatableSector;
			entry.ending_lba = nextAllocatableSector + sz_part - 1;
			nextAllocatableSector += sz_part;

			unsigned i{0};
			auto namePtr = partitionSpec->name.c_str();
			while(i < ARRAY_SIZE(entry.partition_name) && *namePtr != '\0') {
				auto dc = IFS::FAT::tchar2uni(&namePtr);
				auto wc = (dc < 0x10000) ? IFS::FAT::ff_uni2oem(dc, FAT_CODEPAGE) : 0U;
				entry.partition_name[i++] = wc;
			}
		}

		// Write the buffer if it is filled up
		if(partitionIndex + 1 == entriesPerSector) {
			// Update cumulative partition entry checksum
			bcc = crc32(bcc, entries, sectorSize);
			// Write to primary table
			auto entryRelativeSector = partitionIndex / entriesPerSector;
			if(!writeSectors(2 + entryRelativeSector, entries, 1)) {
				return Error::WriteFailure;
			}
			// Write to secondary table
			if(!writeSectors(backupPartitionTableSector + entryRelativeSector, entries, 1)) {
				return Error::WriteFailure;
			}
		}
	}

	/* Create primary GPT header */
	auto& header = workBuffer.as<gpt_header_t>();
	header = gpt_header_t{
		.signature = GPT_HEADER_SIGNATURE,
		.revision = GPT_HEADER_REVISION_V1,
		.header_size = sizeof(gpt_header_t),
		.my_lba = 1,
		.alternate_lba = driveSectors - 1,
		.first_usable_lba = 2 + numPartitionTableSectors,
		.last_usable_lba = backupPartitionTableSector - 1,
		.partition_entry_lba = 2,
		.num_partition_entries = GPT_ITEMS,
		.sizeof_partition_entry = sizeof(gpt_entry_t),
		.partition_entry_array_crc32 = bcc,
	};
	os_get_random(&header.disk_guid, sizeof(efi_guid_t));
	header.header_crc32 = crc32(&header, sizeof(header));
	if(!writeSectors(header.my_lba, &header, 1)) {
		return Error::WriteFailure;
	}

	/* Create secondary GPT header */
	std::swap(header.my_lba, header.alternate_lba);
	header.partition_entry_lba = backupPartitionTableSector;
	header.header_crc32 = 0;
	header.header_crc32 = crc32(&header, sizeof(header));
	if(!writeSectors(header.my_lba, &header, 1)) {
		return Error::WriteFailure;
	}

	/* Create protective MBR */
	auto& mbr = workBuffer.as<legacy_mbr_t>();
	mbr = legacy_mbr_t{
		.partition_record = {{
			.boot_indicator = 0,
			.start_head = 0,
			.start_sector = 2,
			.start_track = 0,
			.os_type = EFI_PMBR_OSTYPE_EFI_GPT,
			.end_head = 0xfe,
			.end_sector = 0xff,
			.end_track = 0,
			.starting_lba = 1,
			.size_in_lba = 0xffffffff,
		}},
		.signature = MSDOS_MBR_SIGNATURE,
	};
	if(!writeSectors(0, &mbr, 1)) {
		return Error::WriteFailure;
	}

	return Error::Success;
}

} // namespace GPT
#endif // FF_LBA64

namespace MBR
{
/* Create partitions in MBR format */
ErrorCode createPartition(Device& device, PartitionSpec* partitionSpec, size_t partitionCount)
{
	if(partitionSpec == nullptr || partitionCount == 0 || partitionCount > 4) {
		return Error::BadParam;
	}

	uint16_t sectorSize;
#if FF_MAX_SS != FF_MIN_SS
	sectorSize = device.getSectorSize();
	if(sectorSize > FF_MAX_SS || sectorSize < FF_MIN_SS || !isLog2(sectorSize)) {
		return Error::BadParam;
	}
#else
	sectorSize = FF_MAX_SS;
#endif
	uint8_t sectorSizeShift = getSizeBits(sectorSize);

	/* Get working buffer */
	WorkBuffer workBuffer(sectorSize, 1);
	if(!workBuffer) {
		return Error::NoMem;
	}

	uint32_t numDeviceSectors = device.getSectorCount();
	// Determine drive CHS without any consideration of the drive geometry
	constexpr uint8_t sectorsPerTrack = N_SEC_TRACK;
	uint8_t numHeads;
	for(numHeads = 8; numHeads != 0 && numDeviceSectors / (numHeads * sectorsPerTrack) > 1024; numHeads *= 2) {
	}
	if(numHeads == 0) {
		// Number of heads needs to be < 256
		numHeads = 255;
	}

	workBuffer.clear();
	auto& mbr = workBuffer.as<legacy_mbr_t>();

	uint32_t sect = sectorsPerTrack;
	for(unsigned i = 0; i < partitionCount && sect < numDeviceSectors; ++i) {
		uint32_t numPartSectors = partitionSpec->size >> sectorSizeShift;
		if(numPartSectors <= 100) {
			// Size as percentage
			numPartSectors = (numPartSectors == 100) ? numDeviceSectors : numPartSectors * (numDeviceSectors / 100);
		}
		if(sect + numPartSectors > numDeviceSectors || sect + numPartSectors < sect) {
			// Clip at drive size
			numPartSectors = numDeviceSectors - sect;
		}
		if(numPartSectors == 0) {
			// End of table or no sector to allocate
			break;
		}

		struct CHS {
			uint8_t head;
			uint8_t sector;
			uint8_t track;
		};

		auto calc_CHS = [&](uint32_t sect) -> CHS {
			unsigned tracks = sect / sectorsPerTrack;
			uint8_t sec = 1 + (sect % sectorsPerTrack);
			unsigned cyl = tracks / numHeads;
			uint8_t head = tracks % numHeads;
			sec |= (cyl >> 2) & 0xC0;
			return CHS{head, sec, uint8_t(cyl)};
		};

		auto start = calc_CHS(sect);
		auto end = calc_CHS(sect + numPartSectors - 1);

		mbr.partition_record[i] = gpt_mbr_record_t{
			.start_head = start.head,
			.start_sector = start.sector,
			.start_track = start.track,
			.os_type = partitionSpec->sysIndicator,
			.end_head = end.head,
			.end_sector = end.sector,
			.end_track = end.track,
			.starting_lba = sect,
			.size_in_lba = numPartSectors,
		};

		sect += numPartSectors;
	}

	mbr.signature = MSDOS_MBR_SIGNATURE;
	return device.write(0, &mbr, sectorSize) ? Error::Success : Error::WriteFailure;
}

} // namespace MBR
} // namespace Storage
