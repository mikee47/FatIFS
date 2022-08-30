/**
 * FileSystem.cpp
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

#include "include/IFS/FatFS/FileSystem.h"
#include "include/IFS/FatFS/Error.h"
#include <IFS/Util.h>
#include <SystemClock.h>

#define SECTOR_SIZE FATFS_SECTOR_SIZE

namespace
{
IFS::FAT::FileSystem* volumes[FATFS_MAX_VOLUMES];

int findVolume(IFS::FAT::FileSystem* fs)
{
	for(unsigned i = 0; i < FATFS_MAX_VOLUMES; ++i) {
		if(volumes[i] == fs) {
			return i;
		}
	}

	return -1;
}

int allocateVolume(IFS::FAT::FileSystem* fs)
{
	for(unsigned i = 0; i < FATFS_MAX_VOLUMES; ++i) {
		if(volumes[i] == nullptr) {
			volumes[i] = fs;
			return i;
		}
	}

	return -1;
}

int deallocateVolume(IFS::FAT::FileSystem* fs)
{
	int pdrv = findVolume(fs);
	if(pdrv >= 0) {
		volumes[pdrv] = nullptr;
	}
	return pdrv;
}

IFS::FAT::FileSystem* getVolume(BYTE pdrv)
{
	auto fs = (pdrv < FATFS_MAX_VOLUMES) ? volumes[pdrv] : nullptr;
	if(fs == nullptr) {
		debug_e("[FAT] Bad drive %u", pdrv);
	}
	return fs;
}

int sysError(FRESULT res)
{
	using namespace IFS;

	switch(res) {
	case FR_DISK_ERR:
		return Error::WriteFailure;
	case FR_NOT_READY:
		return Error::NotMounted;
	case FR_NO_FILE:
	case FR_NO_PATH:
		return Error::NotFound;
	case FR_INVALID_NAME:
		return Error::BadParam;
	case FR_DENIED:
	case FR_EXIST:
	case FR_WRITE_PROTECTED:
		return Error::ReadOnly;
	case FR_INVALID_OBJECT:
		return Error::BadObject;
	case FR_INVALID_DRIVE:
		return Error::BadVolumeIndex;
	case FR_NOT_ENABLED:
		return Error::NoMem;
	case FR_NO_FILESYSTEM:
		return Error::BadFileSystem;
	case FR_TOO_MANY_OPEN_FILES:
		return Error::OutOfFileDescs;
	case FR_TIMEOUT:
	case FR_LOCKED:
	case FR_NOT_ENOUGH_CORE:
	case FR_MKFS_ABORTED:
	default:
		return Error::fromSystem(res);
	}
}

IFS::FileAttributes getAttr(BYTE attr)
{
	using namespace IFS;
	FileAttributes res;
	if(attr & AM_RDO) {
		res += FileAttribute::ReadOnly;
	}
	if(attr & AM_DIR) {
		res += FileAttribute::Directory;
	}
	if(attr & AM_ARC) {
		res += FileAttribute::Archive;
	}
	return res;
}

} // namespace

DWORD get_fattime(void)
{
	if(!SystemClock.isSet()) {
		return 0;
	}

	DateTime dt = SystemClock.now(eTZ_Local);

	union FatTime {
		struct {
			uint32_t second : 5;
			uint32_t minute : 6;
			uint32_t hour : 5;
			uint32_t day : 5;
			uint32_t month : 4;
			uint32_t year : 7;
		};
		uint32_t value;
	};

	static_assert(sizeof(FatTime) == 4, "Bad FatTime");

	FatTime ft{{
		.second = dt.Second / 2U,
		.minute = dt.Minute,
		.hour = dt.Hour,
		.day = dt.Day,
		.month = dt.Month + 1U,
		.year = dt.Year - 1980U,
	}};

	return ft.value;
}

DSTATUS disk_status(BYTE pdrv)
{
	auto fs = getVolume(pdrv);
	return (fs == nullptr) ? STA_NOINIT : 0;
}

DSTATUS disk_initialize(BYTE pdrv)
{
	auto fs = getVolume(pdrv);
	return (fs == nullptr) ? STA_NOINIT : 0;
}

DRESULT disk_read(BYTE pdrv, BYTE* buff, LBA_t sector, UINT count)
{
	auto fs = getVolume(pdrv);
	return fs ? fs->read_sector(buff, sector, count) : RES_PARERR;
}

DRESULT disk_write(BYTE pdrv, const BYTE* buff, LBA_t sector, UINT count)
{
	auto fs = getVolume(pdrv);
	return fs ? fs->write_sector(buff, sector, count) : RES_PARERR;
}

DRESULT disk_ioctl(BYTE pdrv, BYTE cmd, void* buff)
{
	auto fs = getVolume(pdrv);
	return fs ? fs->ioctl(cmd, buff) : RES_PARERR;
}

namespace IFS
{
namespace FAT
{
/**
 * @brief Fat directory object
 */
struct FileDir {
	DIR dir;
};

DRESULT FileSystem::read_sector(void* buff, LBA_t sector, UINT count)
{
	auto addr = sector * SECTOR_SIZE;
	auto size = count * SECTOR_SIZE;
	if(!partition.read(addr, buff, size)) {
		return RES_ERROR;
	}
	if(profiler != nullptr) {
		profiler->read(addr, buff, size);
	}
	return RES_OK;
}

DRESULT FileSystem::write_sector(const void* buff, LBA_t sector, UINT count)
{
	auto addr = sector * SECTOR_SIZE;
	auto size = count * SECTOR_SIZE;
	if(profiler != nullptr) {
		profiler->write(addr, buff, size);
	}
	return partition.write(addr, buff, size) ? RES_OK : RES_ERROR;
}

DRESULT FileSystem::ioctl(BYTE cmd, void* buff)
{
	switch(cmd) {
	case CTRL_SYNC:
		return RES_OK;
	case GET_SECTOR_COUNT:
		*reinterpret_cast<DWORD*>(buff) = partition.size() / SECTOR_SIZE;
		return RES_OK;
	case GET_SECTOR_SIZE:
		*reinterpret_cast<WORD*>(buff) = SECTOR_SIZE;
		return RES_OK;
	case GET_BLOCK_SIZE:
		*reinterpret_cast<DWORD*>(buff) = partition.getBlockSize() / SECTOR_SIZE;
		return RES_OK;
	}

	return RES_ERROR;
}

/**
 * @brief map IFS OpenFlags to LFS equivalents
 * @param flags
 * @param sflags OUT the LFS file open flags
 * @retval OpenFlags if non-zero then some flags weren't recognised
 */
OpenFlags mapFileOpenFlags(OpenFlags flags, BYTE& mode)
{
	BYTE resultmode = 0;

	auto map = [&](OpenFlag flag, BYTE mask) {
		if(flags[flag]) {
			resultmode |= mask;
			flags -= flag;
		}
	};

	map(OpenFlag::Append, FA_OPEN_APPEND);
	map(OpenFlag::Truncate, FA_CREATE_ALWAYS);
	map(OpenFlag::Create, FA_CREATE_NEW);
	map(OpenFlag::Read, FA_READ);
	map(OpenFlag::Write, FA_WRITE);

	flags -= OpenFlag::NoFollow;

	if(flags.any()) {
		debug_w("Unknown OpenFlags: 0x%02X", flags.value());
	}

	mode = resultmode;
	return flags;
}

#define CHECK_MOUNTED()                                                                                                \
	if(!mounted) {                                                                                                     \
		return Error::NotMounted;                                                                                      \
	}

#define GET_FD()                                                                                                       \
	CHECK_MOUNTED()                                                                                                    \
	if(file < FATFS_HANDLE_MIN || file > FATFS_HANDLE_MAX) {                                                           \
		return Error::InvalidHandle;                                                                                   \
	}                                                                                                                  \
	auto& fd = fileDescriptors[file - FATFS_HANDLE_MIN];                                                               \
	if(fd == nullptr) {                                                                                                \
		return Error::FileNotOpen;                                                                                     \
	}

#define CHECK_WRITE()                                                                                                  \
	// 	if(!fd->flags[FileDescriptor::Flag::Write]) {                                                                      \
// 		return Error::ReadOnly;                                                                                        \
// 	}

FileSystem::~FileSystem()
{
	deallocateVolume(this);
}

int FileSystem::mount()
{
	if(!partition) {
		return Error::NoPartition;
	}

	if(partition.type() != Storage::Partition::Type::data) {
		return Error::BadPartition;
	}

	auto res = tryMount();
	if(res < 0) {
		/*
		 * Mount failed, so we either try to repair the system or format it.
		 * For now, just format it.
		 */
		debug_w("[FAT] Mount failed, formatting");
		format();
		res = tryMount();
	}

	return res;
}

int FileSystem::tryMount()
{
	assert(!mounted);

	int pdrv = findVolume(this);
	if(pdrv < 0) {
		pdrv = allocateVolume(this);
		if(pdrv < 0) {
			return Error::NoMem;
		}
	}

	auto res = f_mount(&fatfs, "", 0);
	if(res != FR_OK) {
		int err = sysError(res);
		debug_ifserr(err, "f_mount()");
		return err;
	}

	// get_attr("", AttributeTag::ReadAce, rootAcl.readAccess);
	// get_attr("", AttributeTag::WriteAce, rootAcl.writeAccess);

	driveIndex = pdrv;
	mounted = true;
	return FS_OK;
}

/*
 * Format the file system and leave it mounted in an accessible state.
 */
int FileSystem::format()
{
	auto wasMounted = mounted;
	mounted = false;

	BYTE work_area[FF_MAX_SS];
	MKFS_PARM opt{FM_ANY};
	auto fr = f_mkfs("", &opt, work_area, sizeof(work_area));
	if(fr != FR_OK) {
		auto err = sysError(fr);
		debug_ifserr(err, "format()");
		return err;
	}

	// Re-mount
	return wasMounted ? tryMount() : true;
}

int FileSystem::check()
{
	return Error::NotImplemented;
}

int FileSystem::getinfo(Info& info)
{
	info.clear();
	info.partition = partition;
	switch(fatfs.fs_type) {
	case FS_FAT12:
	case FS_FAT16:
		info.type = Type::Fat;
		break;
	case FS_FAT32:
		info.type = Type::Fat32;
		break;
	case FS_EXFAT:
		info.type = Type::ExFat;
		break;
	default:
		info.type = Type::Unknown;
	}
	info.maxNameLength = FF_MAX_LFN;
	info.maxPathLength = UINT16_MAX;
	if(mounted) {
		info.attr |= Attribute::Mounted;
		info.volumeSize = partition.size();
		info.freeSpace = fatfs.free_clst * fatfs.csize * SECTOR_SIZE;
	}

	return FS_OK;
}

int FileSystem::setProfiler(IProfiler* profiler)
{
	this->profiler = profiler;
	return FS_OK;
}

String FileSystem::getErrorString(int err)
{
	if(Error::isSystem(err)) {
		return fatfsErrorToStr(FRESULT(Error::toSystem(err)));
	} else {
		return Error::toString(err);
	}
}

FileHandle FileSystem::open(const char* path, OpenFlags flags)
{
	CHECK_MOUNTED()
	FS_CHECK_PATH(path)

	// If file is marked read-only, fail write requests
	// if(flags[OpenFlag::Write]) {
	// 	FileAttributes attr;
	// 	get_attr(path ?: "", AttributeTag::FileAttributes, attr);
	// 	if(attr[FileAttribute::ReadOnly]) {
	// 		return Error::ReadOnly;
	// 	}
	// }

	BYTE mode;
	if(mapFileOpenFlags(flags, mode).any()) {
		return FileHandle(Error::NotSupported);
	}

	/*
	 * Allocate a file descriptor
	 */
	int file{Error::OutOfFileDescs};
	for(unsigned i = 0; i < FATFS_MAX_FDS; ++i) {
		auto& fd = fileDescriptors[i];
		if(!fd) {
			fd.reset(new FileDescriptor);
			file = FATFS_HANDLE_MIN + i;
			break;
		}
	}
	if(file < 0) {
		debug_ifserr(file, "open('%s')", path);
		return file;
	}

	auto& fd = fileDescriptors[file - FATFS_HANDLE_MIN];
	FRESULT fr = f_open(&fd->fil, path, mode);
	if(fr != FR_OK) {
		int err = sysError(fr);
		debug_d("open('%s'): %s", path, getErrorString(file).c_str());
		fd.reset();
		return err;
	}

	// get_attr(fd->fil, AttributeTag::ModifiedTime, fd->mtime);

	// if(isRootPath(path)) {
	// 	fd->flags += FileDescriptor::Flag::IsRoot;
	// }
	// fd->flags[FileDescriptor::Flag::Write] = flags[OpenFlag::Write];

	// Copy name into descriptor
	if(path != nullptr) {
		const char* p = strrchr(path, '/');
		if(p == nullptr) {
			p = path;
		} else {
			++p;
		}
		fd->name = p;
	}

	return file;
}

int FileSystem::close(FileHandle file)
{
	GET_FD()

	FRESULT fr = f_close(&fd->fil);
	fd.reset();
	return sysError(fr);
}

int FileSystem::eof(FileHandle file)
{
	GET_FD()

	auto size = fd->fil.obj.objsize;
	auto pos = fd->fil.fptr;

	return (pos >= size) ? 1 : 0;
}

int32_t FileSystem::tell(FileHandle file)
{
	GET_FD()

	return fd->fil.fptr;
}

int FileSystem::ftruncate(FileHandle file, size_t new_size)
{
	GET_FD()
	CHECK_WRITE()

	FRESULT fr = f_lseek(&fd->fil, new_size);
	if(fr == FR_OK) {
		fr = f_truncate(&fd->fil);
	}

	return sysError(fr);
}

int FileSystem::flush(FileHandle file)
{
	GET_FD()
	CHECK_WRITE()

	FRESULT fr = f_sync(&fd->fil);
	return sysError(fr);
}

int FileSystem::read(FileHandle file, void* data, size_t size)
{
	GET_FD()

	UINT res{0};
	FRESULT fr = f_read(&fd->fil, data, size, &res);
	if(fr != FR_OK) {
		int err = sysError(fr);
		debug_ifserr(err, "read()");
		return err;
	}

	debug_i("read(): %u", res);

	return res;
}

int FileSystem::write(FileHandle file, const void* data, size_t size)
{
	GET_FD()
	CHECK_WRITE()

	UINT res{0};
	FRESULT fr = f_write(&fd->fil, data, size, &res);
	if(res < 0) {
		int err = sysError(fr);
		debug_ifserr(err, "write()");
		return err;
	}

	fd->touch();
	return res;
}

int FileSystem::lseek(FileHandle file, int offset, SeekOrigin origin)
{
	GET_FD()

	switch(origin) {
	case SeekOrigin::Start:
		break;
	case SeekOrigin::Current:
		offset += fd->fil.fptr;
		break;
	case SeekOrigin::End:
		offset += fd->fil.obj.objsize;
		break;
	default:
		return Error::BadParam;
	}

	FRESULT fr = f_lseek(&fd->fil, offset);
	return (fr == FR_OK) ? offset : sysError(fr);
}

int FileSystem::stat(const char* path, Stat* stat)
{
	CHECK_MOUNTED()
	FS_CHECK_PATH(path);

	FILINFO inf;
	FRESULT fr = f_stat(path, &inf);
	if(fr != FR_OK) {
		return sysError(fr);
	}

	if(stat == nullptr) {
		return FS_OK;
	}

	*stat = Stat{};
	stat->fs = this;
	stat->size = inf.fsize;
	stat->acl = rootAcl;
	stat->attr = getAttr(inf.fattrib);
	stat->name.copy(inf.fname);

	return FS_OK;
}

int FileSystem::fstat(FileHandle file, Stat* stat)
{
	GET_FD()

	if(stat == nullptr) {
		return FS_OK;
	}

	*stat = Stat{};
	stat->fs = this;
	stat->size = fd->fil.obj.objsize;
	stat->acl = rootAcl;
	stat->attr = getAttr(fd->fil.obj.attr);
	stat->name.copy(fd->name.c_str());
	// stat->mtime = fd->mtime;

	return FS_OK;
}

int FileSystem::fsetxattr(FileHandle file, AttributeTag tag, const void* data, size_t size)
{
	return Error::NotImplemented;
}

int FileSystem::fgetxattr(FileHandle file, AttributeTag tag, void* buffer, size_t size)
{
	return Error::NotImplemented;
}

int FileSystem::fenumxattr(FileHandle file, AttributeEnumCallback callback, void* buffer, size_t bufsize)
{
	return Error::NotImplemented;
}

int FileSystem::setxattr(const char* path, AttributeTag tag, const void* data, size_t size)
{
	return Error::NotImplemented;
}

int FileSystem::getxattr(const char* path, AttributeTag tag, void* buffer, size_t size)
{
	return Error::NotImplemented;
}

int FileSystem::opendir(const char* path, DirHandle& dir)
{
	CHECK_MOUNTED()
	FS_CHECK_PATH(path)

	auto d = new FileDir;
	if(d == nullptr) {
		return Error::NoMem;
	}

	FRESULT fr = f_opendir(&d->dir, path);
	if(fr != FR_OK) {
		int err = sysError(fr);
		delete d;
		return err;
	}

	// lfs_dir_seek(&lfs, &d->dir, 2);

	dir = DirHandle(d);
	return FS_OK;
}

int FileSystem::rewinddir(DirHandle dir)
{
	GET_FILEDIR()

	FRESULT fr = f_readdir(&d->dir, nullptr);
	return sysError(fr);
}

int FileSystem::readdir(DirHandle dir, Stat& stat)
{
	GET_FILEDIR()

	FILINFO inf;
	FRESULT fr = f_readdir(&d->dir, &inf);
	if(fr != FR_OK) {
		return sysError(fr);
	}
	if(inf.fname[0] == '\0') {
		return Error::NoMoreFiles;
	}

	stat = Stat{};
	stat.fs = this;
	stat.size = inf.fsize;
	stat.acl = rootAcl;
	stat.attr = getAttr(inf.fattrib);
	stat.name.copy(inf.fname);

	return FS_OK;
}

int FileSystem::closedir(DirHandle dir)
{
	GET_FILEDIR()

	FRESULT fr = f_closedir(&d->dir);
	delete d;
	return sysError(fr);
}

int FileSystem::mkdir(const char* path)
{
	CHECK_MOUNTED()
	if(isRootPath(path)) {
		return Error::BadParam;
	}

	FRESULT fr = f_mkdir(path);
	return sysError(fr);
}

int FileSystem::rename(const char* oldpath, const char* newpath)
{
	CHECK_MOUNTED()
	if(isRootPath(oldpath) || isRootPath(newpath)) {
		return Error::BadParam;
	}

	FRESULT fr = f_rename(oldpath, newpath);
	return sysError(fr);
}

int FileSystem::remove(const char* path)
{
	CHECK_MOUNTED()
	if(isRootPath(path)) {
		return Error::BadParam;
	}

	// Check file is not marked read-only
	// FileAttributes attr{};
	// get_attr(path, AttributeTag::FileAttributes, attr);
	// if(attr[FileAttribute::ReadOnly]) {
	// 	return Error::ReadOnly;
	// }

	FRESULT fr = f_unlink(path);
	return sysError(fr);
}

int FileSystem::fremove(FileHandle file)
{
	return Error::NotImplemented;
}

} // namespace FAT
} // namespace IFS
