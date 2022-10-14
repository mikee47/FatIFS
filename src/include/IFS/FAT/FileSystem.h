/****
 * FileSystem.h - Provides an IFS FileSystem implementation for FAT.
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
 */

#pragma once

#include <IFS/FileSystem.h>

namespace IFS
{
namespace FAT
{
// File handles start at this value
#ifndef FATFS_HANDLE_MIN
#define FATFS_HANDLE_MIN 300
#endif

// Maximum number of file descriptors
#ifndef FATFS_MAX_FDS
#define FATFS_MAX_FDS 5
#endif

// Maximum file handle value
#define FATFS_HANDLE_MAX (FATFS_HANDLE_MIN + FATFS_MAX_FDS - 1)

struct S_FATFS;
struct S_FILINFO;
struct FileDescriptor;

/**
 * Wraps fatfs
 */
class FileSystem : public IFileSystem
{
public:
	FileSystem(Storage::Partition partition);
	~FileSystem();

	int mount() override;
	int getinfo(Info& info) override;
	int setProfiler(IProfiler* profiler) override;
	String getErrorString(int err) override;
	int opendir(const char* path, DirHandle& dir) override;
	int readdir(DirHandle dir, Stat& stat) override;
	int rewinddir(DirHandle dir) override;
	int closedir(DirHandle dir) override;
	int mkdir(const char* path) override;
	int stat(const char* path, Stat* stat) override;
	int fstat(FileHandle file, Stat* stat) override;
	int fsetxattr(FileHandle file, AttributeTag tag, const void* data, size_t size) override;
	int fgetxattr(FileHandle file, AttributeTag tag, void* buffer, size_t size) override;
	int fenumxattr(FileHandle file, AttributeEnumCallback callback, void* buffer, size_t bufsize) override;
	int setxattr(const char* path, AttributeTag tag, const void* data, size_t size) override;
	int getxattr(const char* path, AttributeTag tag, void* buffer, size_t size) override;
	FileHandle open(const char* path, OpenFlags flags) override;
	int close(FileHandle file) override;
	int read(FileHandle file, void* data, size_t size) override;
	int write(FileHandle file, const void* data, size_t size) override;
	file_offset_t lseek(FileHandle file, file_offset_t offset, SeekOrigin origin) override;
	int eof(FileHandle file) override;
	file_offset_t tell(FileHandle file) override;
	int ftruncate(FileHandle file, file_size_t new_size) override;
	int flush(FileHandle file) override;
	int rename(const char* oldpath, const char* newpath) override;
	int remove(const char* path) override;
	int fremove(FileHandle file) override;
	int format() override;
	int check() override;

	bool read_sector(void* buff, uint32_t sector, size_t count);
	bool write_sector(const void* buff, uint32_t sector, size_t count);
	bool ioctl(uint8_t cmd, void* buff);

	S_FATFS* getFatFS() const
	{
		return fatfs.get();
	}

private:
	void fillStat(Stat& stat, const S_FILINFO& inf);

	Storage::Partition partition;
	IProfiler* profiler{nullptr};
	std::unique_ptr<S_FATFS> fatfs;
	std::unique_ptr<FileDescriptor> fileDescriptors[FATFS_MAX_FDS];
	ACL rootAcl{};
	uint8_t sectorSizeShift{0};
};

} // namespace FAT
} // namespace IFS
