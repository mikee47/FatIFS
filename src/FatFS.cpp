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

#include "include/IFS/FatFS.h"
#include "include/IFS/FatFS/FileSystem.h"
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

bool fatfs_mount()
{
	using SubType = Storage::Partition::SubType::Data;
	for(auto part : Storage::findPartition(Storage::Partition::Type::data)) {
		switch(SubType(part.subType())) {
		case SubType::fat:
		case SubType::fat32:
		case SubType::exfat:
			return fatfs_mount(part);
		default:
			break;
		}
	}

	return false;
}

bool fatfs_mount(Storage::Partition partition)
{
	auto fs = IFS::createFatFilesystem(partition);
	return fileMountFileSystem(fs);
}
