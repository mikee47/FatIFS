/****
 * FatIFS.cpp
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

#include "include/IFS/FAT.h"
#include "include/IFS/FAT/FileSystem.h"
#include <FileSystem.h>
#include <Storage.h>

namespace IFS
{
FileSystem* createFatFilesystem(Storage::Partition partition)
{
	auto fs = new FAT::FileSystem(partition);
	return FileSystem::cast(fs);
}

} // namespace IFS
