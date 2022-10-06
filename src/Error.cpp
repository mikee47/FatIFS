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

#include "include/IFS/FAT/Error.h"
#include <IFS/Error.h>
#include "../fatfs/ff.h"

#define FATFS_RESULT_TRANSLATION_MAP(XX)                                                                               \
	XX(NOT_READY, Error::NotMounted, "The physical drive cannot work")                                                 \
	XX(NO_FILE, Error::NotFound, "Could not find the file")                                                            \
	XX(NO_PATH, Error::NotFound, "Could not find the path")                                                            \
	XX(INVALID_NAME, Error::BadParam, "The path name format is invalid")                                               \
	XX(DENIED, Error::Denied, "Access denied due to prohibited access or directory full")                              \
	XX(EXIST, Error::Exists, "Access denied due to prohibited access")                                                 \
	XX(INVALID_OBJECT, Error::BadObject, "The file/directory object is invalid")                                       \
	XX(WRITE_PROTECTED, Error::ReadOnly, "The physical drive is write protected")                                      \
	XX(NOT_ENABLED, Error::NotMounted, "The volume has no work area")                                                  \
	XX(NO_FILESYSTEM, Error::BadFileSystem, "There is no valid FAT volume")                                            \
	XX(NOT_ENOUGH_CORE, Error::NoMem, "LFN working buffer could not be allocated")                                     \
	XX(TOO_MANY_OPEN_FILES, Error::OutOfFileDescs, "Number of open files > FF_FS_LOCK")                                \
	XX(INVALID_PARAMETER, Error::BadParam, "Given parameter is invalid")                                               \
	XX(NO_SPACE, Error::NoSpace, "No space")                                                                           \
	XX(FILE_TOO_BIG, Error::TooBig, "File is too big")

#define FATFS_RESULT_MAP(XX)                                                                                           \
	XX(INT_ERR, 2, "Assertion failed")                                                                                 \
	XX(INVALID_DRIVE, 11, "The logical drive number is invalid")                                                       \
	XX(MKFS_ABORTED, 14, "The f_mkfs() aborted due to any problem")                                                    \
	XX(TIMEOUT, 15, "Could not get a grant to access the volume within defined period")

namespace IFS
{
namespace FAT
{
int translateFatfsResult(uint8_t result, bool diskio_write)
{
	switch(result) {
	case FR_DISK_ERR:
		return diskio_write ? Error::WriteFailure : Error::ReadFailure;
#define XX(res, err, ...)                                                                                              \
	case FR_##res:                                                                                                     \
		return err;
		FATFS_RESULT_TRANSLATION_MAP(XX)
#undef XX
	default:
		return Error::fromSystem(-result);
	}
}

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
