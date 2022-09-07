/**
 * Error.cpp
 *
 * Copyright 2022 mikee47 <mike@sillyhouse.net>
 *
 * This file is part of the FatIFS Library
 *
 * This library is free software: you can redistribute it and/or modify it under the terms of the
 * GNU General Public License as published by the Free Software Foundation, version 3 or later.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with this library.
 * If not, see <https://www.gnu.org/licenses/>.
 *
 ****/

#include "include/IFS/FatFS/Error.h"
#include "fatfs/ff.h"

#define FATFS_RESULT_MAP(XX)                                                                                           \
	XX(OK, 0, "Succeeded")                                                                                             \
	XX(DISK_ERR, 1, "A hard error occurred in the low level disk I/O layer")                                           \
	XX(INT_ERR, 2, "Assertion failed")                                                                                 \
	XX(NOT_READY, 3, "The physical drive cannot work")                                                                 \
	XX(NO_FILE, 4, "Could not find the file")                                                                          \
	XX(NO_PATH, 5, "Could not find the path")                                                                          \
	XX(INVALID_NAME, 6, "The path name format is invalid")                                                             \
	XX(DENIED, 7, "Access denied due to prohibited access or directory full")                                          \
	XX(EXIST, 8, "Access denied due to prohibited access")                                                             \
	XX(INVALID_OBJECT, 9, "The file/directory object is invalid")                                                      \
	XX(WRITE_PROTECTED, 10, "The physical drive is write protected")                                                   \
	XX(INVALID_DRIVE, 11, "The logical drive number is invalid")                                                       \
	XX(NOT_ENABLED, 12, "The volume has no work area")                                                                 \
	XX(NO_FILESYSTEM, 13, "There is no valid FAT volume")                                                              \
	XX(MKFS_ABORTED, 14, "The f_mkfs() aborted due to any problem")                                                    \
	XX(TIMEOUT, 15, "Could not get a grant to access the volume within defined period")                                \
	XX(LOCKED, 16, "The operation is rejected according to the file sharing policy")                                   \
	XX(NOT_ENOUGH_CORE, 17, "LFN working buffer could not be allocated")                                               \
	XX(TOO_MANY_OPEN_FILES, 18, "Number of open files > FF_FS_LOCK")                                                   \
	XX(INVALID_PARAMETER, 19, "Given parameter is invalid")

namespace IFS
{
namespace FAT
{
String fatfsErrorToStr(uint8_t res)
{
	switch(res) {
#define XX(tag, ...)                                                                                                   \
	case FR_##tag:                                                                                                     \
		return F(#tag);
		FATFS_RESULT_MAP(XX)
#undef XX
	default:
		return String(res);
	}
}
} // namespace FAT
} // namespace IFS
