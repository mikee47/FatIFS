/****
 * FAT.h
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

#pragma once

#include <IFS/FileSystem.h>
#include "FAT/Format.h"

namespace IFS
{
/**
 * @brief Create a FAT filesystem
 * @param partition
 * @retval FileSystem* constructed filesystem object
 */
FileSystem* createFatFilesystem(Storage::Partition partition);

} // namespace IFS
