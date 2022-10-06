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

#include "include/IFS/FAT/FileSystem.h"
#include "include/IFS/FAT/Error.h"
#include "include/IFS/FAT/FatTime.h"
#include "include/IFS/FAT/Format.h"
#include <IFS/Util.h>
#include <SystemClock.h>

namespace
{
IFS::FAT::FileSystem* currentVolume;
bool diskio_write;
} // namespace

namespace IFS
{
namespace FAT
{
#include "../fatfs/ff.h"
#include "../fatfs/diskio.h"

struct S_FATFS : public FATFS {
};

struct S_FILINFO : public FILINFO {
};

/**
 * @brief Details for an open file
 */
struct FileDescriptor {
	CString name;
	FIL fil{};

	void touch()
	{
		// TODO
	}
};

/**
 * @brief Fat directory object
 */
struct FileDir {
	DIR dir;
};

DWORD get_fattime(void)
{
#ifndef ARCH_HOST
	if(!SystemClock.isSet()) {
		return 0;
	}
#endif

	DateTime dt = SystemClock.now(eTZ_UTC);
	return FatTime(dt).value;
}

DRESULT disk_read(BYTE pdrv, BYTE* buff, LBA_t sector, UINT count)
{
	(void)pdrv;
	debug_d("%s(sector=%llu, count=%u)", __FUNCTION__, sector, count);
	assert(currentVolume != nullptr);
	diskio_write = false;
	return currentVolume->read_sector(buff, sector, count) ? RES_OK : RES_PARERR;
}

DRESULT disk_write(BYTE pdrv, const BYTE* buff, LBA_t sector, UINT count)
{
	(void)pdrv;
	debug_d("%s(sector=%llu, count=%u)", __FUNCTION__, sector, count);
	assert(currentVolume != nullptr);
	diskio_write = true;
	return currentVolume->write_sector(buff, sector, count) ? RES_OK : RES_PARERR;
}

DRESULT disk_ioctl(BYTE pdrv, BYTE cmd, void* buff)
{
	(void)pdrv;
	debug_d("%s(cmd=%u)", __FUNCTION__, cmd);
	assert(currentVolume != nullptr);
	return currentVolume->ioctl(cmd, buff) ? RES_OK : RES_PARERR;
}

#include "../fatfs/ffunicode.c"
#include "../fatfs/ff.c"

const char* getFatPath(const char* path)
{
	return isRootPath(path) ? "" : path;
}

int sysError(FRESULT res)
{
	return translateFatfsResult(res, diskio_write);
}

FileAttributes getAttr(BYTE attr)
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

bool FileSystem::read_sector(void* buff, uint32_t sector, size_t count)
{
	auto offset = storage_size_t(sector) << sectorSizeShift;
	auto size = count << sectorSizeShift;
	if(!partition.read(offset, buff, size)) {
		return false;
	}
	if(profiler != nullptr) {
		profiler->read(offset, buff, size);
	}
	return true;
}

bool FileSystem::write_sector(const void* buff, uint32_t sector, size_t count)
{
	auto offset = uint64_t(sector) << sectorSizeShift;
	auto size = count << sectorSizeShift;
	if(profiler != nullptr) {
		profiler->write(offset, buff, size);
	}

	return partition.write(offset, buff, size);
}

bool FileSystem::ioctl(uint8_t cmd, void* buff)
{
	switch(cmd) {
	case CTRL_SYNC:
		return partition.sync();
	case CTRL_TRIM: {
		auto rt = static_cast<LBA_t*>(buff);
		return partition.trim(rt[0], rt[1]);
	}
	case GET_SECTOR_COUNT:
		*reinterpret_cast<DWORD*>(buff) = partition.size() >> sectorSizeShift;
		return true;
	case GET_SECTOR_SIZE:
		*reinterpret_cast<WORD*>(buff) = 1U << sectorSizeShift;
		return true;
	case GET_BLOCK_SIZE:
		*reinterpret_cast<DWORD*>(buff) = partition.getBlockSize() >> sectorSizeShift;
		return true;
	default:
		return false;
	}
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

	if(flags[OpenFlag::Create]) {
		if(flags[OpenFlag::Truncate]) {
			resultmode |= FA_CREATE_ALWAYS;
			flags -= OpenFlag::Truncate;
		} else {
			resultmode |= FA_OPEN_ALWAYS;
		}
		flags -= OpenFlag::Create;
	}

	map(OpenFlag::Append, FA_OPEN_APPEND);
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
	if(!fatfs) {                                                                                                       \
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

FileSystem::FileSystem(Storage::Partition partition) : partition(partition)
{
}

FileSystem::~FileSystem()
{
}

int FileSystem::mount()
{
	if(!partition) {
		return Error::NoPartition;
	}

	if(partition.fullType() != Storage::Partition::SubType::Data::fat) {
		return Error::BadPartition;
	}

	sectorSizeShift = Storage::getSizeBits(partition.getSectorSize());

	auto res = tryMount();
	if(res < 0) {
		return res;
	}

	return res;
}

int FileSystem::tryMount()
{
	assert(!fatfs);

	fatfs.reset(new S_FATFS{});
	if(!fatfs) {
		return Error::NoMem;
	}

	currentVolume = this;
	FATFS* rfs;
	auto res = mount_volume(nullptr, &rfs, 1);
	if(res != FR_OK) {
		fatfs.reset();
		int err = sysError(res);
		debug_ifserr(err, "[FAT] tryMount");
		return err;
	}

	// get_attr("", AttributeTag::ReadAce, rootAcl.readAccess);
	// get_attr("", AttributeTag::WriteAce, rootAcl.writeAccess);

	return FS_OK;
}

/*
 * Format the file system and leave it mounted in an accessible state.
 */
int FileSystem::format()
{
	// Use existing FAT variant if available, otherwise any type will do
	auto getSysType = [](uint8_t fs_type) -> SysTypes {
		switch(fs_type) {
		case FS_FAT12:
			return SysType::fat12;
		case FS_FAT16:
			return SysType::fat16;
		case FS_FAT32:
			return SysType::fat32;
		case FS_EXFAT:
			return SysType::exfat;
		default:
			return 0; // Use any FAT type
		}
	};
	SysTypes sysType = fatfs ? getSysType(fatfs->fs_type) : 0;

	fatfs.reset();

	int err = formatVolume(partition, {sysType});
	if(err) {
		debug_ifserr(err, "[FAT] format");
		return err;
	}

	// Re-mount
	return sysType ? tryMount() : FS_OK;
}

int FileSystem::check()
{
	return Error::NotImplemented;
}

int FileSystem::getinfo(Info& info)
{
	info.clear();
	info.partition = partition;
	info.attr = Attribute::NoMeta;
	info.maxNameLength = FF_MAX_LFN;
	info.maxPathLength = UINT16_MAX;
	if(!fatfs) {
		return FS_OK;
	}

	switch(fatfs->fs_type) {
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

	info.attr |= Attribute::Mounted;
	info.volumeSize = partition.size();
	currentVolume = this;
	DWORD nclst;
	FATFS* fs;
	FRESULT fr = f_getfree("", &nclst, &fs);
	if(fr == FR_OK) {
		info.freeSpace = volume_size_t(nclst) * fs->csize << sectorSizeShift;
	}
	char label[32];
	DWORD vsn;
	fr = f_getlabel("", label, &vsn);
	if(fr == FR_OK) {
		info.name.copy(label);
		info.volumeID = vsn;
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
		debug_ifserr(file, "[FAT] open('%s')", path);
		return file;
	}

	auto& fd = fileDescriptors[file - FATFS_HANDLE_MIN];
	currentVolume = this;
	FRESULT fr = f_open(&fd->fil, getFatPath(path), mode);
	if(fr != FR_OK) {
		int err = sysError(fr);
		debug_d("[FAT] open('%s'): %s", path, getErrorString(file).c_str());
		fd.reset();
		return err;
	}

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

	currentVolume = this;
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

file_offset_t FileSystem::tell(FileHandle file)
{
	GET_FD()

	int64_t offset = f_tell(&fd->fil);
	if(file_offset_t(offset) != offset) {
		debug_w("[FAT] Out of range %lld", offset);
		return Error::TooBig;
	}

	return offset;
}

int FileSystem::ftruncate(FileHandle file, file_size_t new_size)
{
	GET_FD()

	currentVolume = this;
	FRESULT fr = f_lseek(&fd->fil, new_size);
	if(fr != FR_OK) {
		return sysError(fr);
	}
	auto curpos = f_tell(&fd->fil);
	if(curpos != new_size) {
		return (fd->fil.flag & FA_WRITE) ? Error::NoSpace : Error::SeekBounds;
	}
	fr = f_truncate(&fd->fil);
	return sysError(fr);
}

int FileSystem::flush(FileHandle file)
{
	GET_FD()

	currentVolume = this;
	FRESULT fr = f_sync(&fd->fil);
	return sysError(fr);
}

int FileSystem::read(FileHandle file, void* data, size_t size)
{
	GET_FD()

	currentVolume = this;
	UINT res{0};
	FRESULT fr = f_read(&fd->fil, data, size, &res);
	if(fr != FR_OK) {
		int err = sysError(fr);
		debug_ifserr(err, "[FAT] read(%u)", size);
		return err;
	}

	return res;
}

int FileSystem::write(FileHandle file, const void* data, size_t size)
{
	GET_FD()

	currentVolume = this;
	UINT res{0};
	FRESULT fr = f_write(&fd->fil, data, size, &res);
	if(fr != FR_OK) {
		int err = sysError(fr);
		debug_ifserr(err, "[FAT] write(%u)", size);
		return err;
	}

	fd->touch();
	return res;
}

file_offset_t FileSystem::lseek(FileHandle file, file_offset_t offset, SeekOrigin origin)
{
	GET_FD()

	int64_t newOffset = offset;
	switch(origin) {
	case SeekOrigin::Start:
		break;
	case SeekOrigin::Current:
		newOffset += fd->fil.fptr;
		break;
	case SeekOrigin::End:
		newOffset += fd->fil.obj.objsize;
		break;
	default:
		return Error::BadParam;
	}

	if(file_offset_t(newOffset) != newOffset) {
		debug_e("[FAT] lseek(%lld)", newOffset);
		return Error::TooBig;
	}
	offset = newOffset;

	if(offset < 0) {
		return Error::SeekBounds;
	}

	currentVolume = this;
	FRESULT fr = f_lseek(&fd->fil, offset);
	if(fr != FR_OK) {
		return sysError(fr);
	}

	auto curpos = f_tell(&fd->fil);
	if(file_offset_t(curpos) == offset) {
		return offset;
	}

	debug_w("[FAT] lseek(%lld) -> %lld", int64_t(curpos), int64_t(offset));

	return (fd->fil.flag & FA_WRITE) ? Error::NoSpace : Error::SeekBounds;
}

void FileSystem::fillStat(Stat& stat, const S_FILINFO& inf)
{
	stat = Stat{};
	stat.fs = this;
	stat.size = inf.fsize;
	stat.acl = rootAcl;
	stat.attr = getAttr(inf.fattrib);
	stat.name.copy(inf.fname);
	stat.mtime = time_t(FatTime(inf.fdate, inf.ftime));
}

int FileSystem::stat(const char* path, Stat* stat)
{
	CHECK_MOUNTED()
	if(isRootPath(path)) {
		if(stat != nullptr) {
			*stat = Stat{};
			stat->fs = this;
			stat->acl = rootAcl;
			stat->attr = FileAttribute::Directory;
			stat->mtime = fsGetTimeUTC();
		}
		return FS_OK;
	}

	S_FILINFO inf;
	currentVolume = this;
	FRESULT fr = f_stat(path, &inf);
	if(fr != FR_OK) {
		return sysError(fr);
	}

	if(stat == nullptr) {
		return FS_OK;
	}

	fillStat(*stat, inf);
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
	stat->mtime = time_t(FatTime(fd->fil.obj.modtime));

	return FS_OK;
}

int FileSystem::fsetxattr(FileHandle file, AttributeTag tag, const void* data, size_t size)
{
	GET_FD()

	if(size != getAttributeSize(tag)) {
		return Error::BadParam;
	}

	bool changed{false};

	switch(tag) {
	case AttributeTag::ModifiedTime: {
		TimeStamp ts;
		memcpy(&ts, data, size);
		fd->fil.obj.modtime = FatTime(ts).value;
		changed = true;
		break;
	}

	case AttributeTag::FileAttributes: {
		FileAttributes attr;
		memcpy(&attr, data, size);
		fd->fil.obj.attr &= ~(AM_ARC | AM_RDO);
		if(attr[FileAttribute::Archive]) {
			fd->fil.obj.attr |= AM_ARC;
			changed = true;
		}
		if(attr[FileAttribute::ReadOnly]) {
			fd->fil.obj.attr |= AM_RDO;
			changed = true;
		}
		break;
	}

	default:
		return Error::NotImplemented;
	}

	if(changed) {
		fd->fil.flag |= FA_MODIFIED;
	}

	return FS_OK;
}

int FileSystem::fgetxattr(FileHandle file, AttributeTag tag, void* buffer, size_t size)
{
	GET_FD()

	switch(tag) {
	case AttributeTag::ModifiedTime: {
		TimeStamp mtime;
		mtime = time_t(FatTime(fd->fil.obj.modtime));
		memcpy(buffer, &mtime, std::min(size, sizeof(mtime)));
		return sizeof(mtime);
	}

	case AttributeTag::FileAttributes: {
		FileAttributes attr = getAttr(fd->fil.obj.attr);
		memcpy(buffer, &attr, std::min(size, sizeof(attr)));
		return sizeof(attr);
	}

	default:
		return Error::NotImplemented;
	}
}

int FileSystem::fenumxattr(FileHandle file, AttributeEnumCallback callback, void* buffer, size_t bufsize)
{
	GET_FD()

	TimeStamp mtime;
	mtime = time_t(FatTime(fd->fil.obj.modtime));
	FileAttributes attr = getAttr(fd->fil.obj.attr);

	AttributeEnum e{buffer, bufsize};
	e.set(AttributeTag::ModifiedTime, mtime);
	if(!callback(e)) {
		return 1;
	}
	e.set(AttributeTag::FileAttributes, attr);
	callback(e);
	return 2;
}

int FileSystem::setxattr(const char* path, AttributeTag tag, const void* data, size_t size)
{
	CHECK_MOUNTED()

	if(data == nullptr) {
		// Attribute deletion
		return Error::NotSupported;
	}

	if(size != getAttributeSize(tag)) {
		return Error::BadParam;
	}

	currentVolume = this;
	path = getFatPath(path);

	switch(tag) {
	case AttributeTag::ModifiedTime: {
		TimeStamp ts;
		memcpy(&ts, data, size);
		FatTime ft(ts);
		FILINFO inf{
			.fdate = ft.date,
			.ftime = ft.time,
		};
		FRESULT fr = f_utime(path, &inf);
		return sysError(fr);
	}

	case AttributeTag::FileAttributes: {
		FileAttributes attr;
		memcpy(&attr, data, size);
		uint8_t fattr{0};
		if(attr[FileAttribute::Archive]) {
			fattr |= AM_ARC;
		}
		if(attr[FileAttribute::ReadOnly]) {
			fattr |= AM_RDO;
		}
		FRESULT fr = f_chmod(path, fattr, AM_ARC | AM_RDO);
		return sysError(fr);
	}

	default:
		return Error::NotImplemented;
	}
}

int FileSystem::getxattr(const char* path, AttributeTag tag, void* buffer, size_t size)
{
	CHECK_MOUNTED()

	currentVolume = this;
	path = getFatPath(path);

	FILINFO inf;
	FRESULT fr = f_stat(path, &inf);
	if(fr != FR_OK) {
		return sysError(fr);
	}

	switch(tag) {
	case AttributeTag::ModifiedTime: {
		TimeStamp mtime;
		mtime = time_t(FatTime(inf.fdate, inf.ftime));
		memcpy(buffer, &mtime, std::min(size, sizeof(mtime)));
		return sizeof(mtime);
	}

	case AttributeTag::FileAttributes: {
		FileAttributes attr = getAttr(inf.fattrib);
		memcpy(buffer, &attr, std::min(size, sizeof(attr)));
		return sizeof(attr);
	}

	default:
		return Error::NotImplemented;
	}
}

int FileSystem::opendir(const char* path, DirHandle& dir)
{
	CHECK_MOUNTED()

	auto d = new FileDir;
	if(d == nullptr) {
		return Error::NoMem;
	}

	currentVolume = this;
	FRESULT fr = f_opendir(&d->dir, getFatPath(path));
	if(fr != FR_OK) {
		int err = sysError(fr);
		delete d;
		return err;
	}

	dir = DirHandle(d);
	return FS_OK;
}

int FileSystem::rewinddir(DirHandle dir)
{
	GET_FILEDIR()

	currentVolume = this;
	FRESULT fr = f_readdir(&d->dir, nullptr);
	return sysError(fr);
}

int FileSystem::readdir(DirHandle dir, Stat& stat)
{
	GET_FILEDIR()

	currentVolume = this;
	S_FILINFO inf;
	FRESULT fr = f_readdir(&d->dir, &inf);
	if(fr != FR_OK) {
		return sysError(fr);
	}
	if(inf.fname[0] == '\0') {
		return Error::NoMoreFiles;
	}

	stat = Stat{};
	stat.fs = this;
	fillStat(stat, inf);
	return FS_OK;
}

int FileSystem::closedir(DirHandle dir)
{
	GET_FILEDIR()

	currentVolume = this;
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

	currentVolume = this;
	FRESULT fr = f_mkdir(path);
	return sysError(fr);
}

int FileSystem::rename(const char* oldpath, const char* newpath)
{
	CHECK_MOUNTED()
	if(isRootPath(oldpath) || isRootPath(newpath)) {
		return Error::BadParam;
	}

	currentVolume = this;
	FRESULT fr = f_rename(getFatPath(oldpath), getFatPath(newpath));
	return sysError(fr);
}

int FileSystem::remove(const char* path)
{
	CHECK_MOUNTED()
	if(isRootPath(path)) {
		return Error::BadParam;
	}

	currentVolume = this;
	FRESULT fr = f_unlink(getFatPath(path));
	return sysError(fr);
}

int FileSystem::fremove(FileHandle file)
{
	return Error::NotImplemented;
}

} // namespace FAT
} // namespace IFS
