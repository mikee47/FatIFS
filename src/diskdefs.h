/*
 * Low-level definitions
 */

#pragma once

#include <cstdint>
#include <cstddef>
#include <sys/pgmspace.h>
#include <Data/Uuid.h>

#define FSTYPE_FAT 0x2020202020544146ULL   // "FAT     " 46 41 54 20 20 20 20 20
#define FSTYPE_FAT32 0x2020203233544146ULL // "FAT32   " 46 41 54 33 32 20 20 20
#define FSTYPE_EXFAT 0x2020205441465845ULL // "EXFAT   " 45 58 46 41 54 20 20 20

#define N_SEC_TRACK 63	 // Sectors per track for determination of drive CHS
#define GPT_ALIGN 0x100000 // Alignment of partitions in GPT [byte] (>=128KB)
#define GPT_ITEMS 128	  // Number of GPT table size (>=128, sector aligned)

#define OSTYPE_EXTENDED 0x05

namespace Storage
{
namespace diskdefs
{
namespace FAT
{
#include "../linux/msdos_fs.h"
}

namespace EXFAT
{
#include "../linux/exfat_raw.h"
}

#include "../linux/efi.h"

template <typename T> T align_up(T value, uint32_t align)
{
	return (value + align - 1) & ~(T(align) - 1);
}

template <typename T> auto getBlockCount(T byteCount, uint32_t blockSize)
{
	return (byteCount + blockSize - 1) / blockSize;
}

uint32_t crc32_byte(uint32_t crc, uint8_t d);
uint32_t crc32(uint32_t bcc, const void* data, size_t length);

inline uint32_t crc32(const void* data, size_t length)
{
	return crc32(0, data, length);
}

// exFAT checksumming
uint32_t xsum32(uint8_t dat, uint32_t sum);
uint32_t xsum32(const void* buffer, size_t length, uint32_t sum);

bool verifyGptHeader(gpt_header_t& gpt);

} // namespace diskdefs
} // namespace Storage
