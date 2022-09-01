#include <SmingCore.h>
#include <IFS/FatFS.h>
#include <IFS/FileCopier.h>
#include <IFS/Debug.h>
#include <IFS/FileSystem.h>
#include <Storage/FileDevice.h>
#include <Storage/DiskDevice.h>
#include <Storage/Debug.h>

namespace
{
DEFINE_FSTR(test_image, "/mnt/c/temp/2017-07-05-raspbian-jessie-lite.img")

Storage::Device* mountTestImage(const String& tag, const String& filename)
{
	auto& hfs = IFS::Host::getFileSystem();
	auto f = hfs.open(filename, IFS::File::ReadOnly);
	if(f < 0) {
		debug_e("Failed to open '%s': %s", filename.c_str(), hfs.getErrorString(f).c_str());
		return nullptr;
	}
	auto dev = new Storage::FileDevice(tag, hfs, f);
	Storage::registerDevice(dev);
	Storage::scanDiskPartitions(*dev);

	return dev;
}

bool fscopy(const char* srcFile)
{
	auto part = Storage::findDefaultPartition(Storage::Partition::SubType::Data::fwfs);
	auto srcfs = IFS::createFirmwareFilesystem(part);
	srcfs->mount();

	auto dstfs = IFS::getDefaultFileSystem();

	IFS::Profiler profiler;
	dstfs->setProfiler(&profiler);
	IFS::FileCopier copier(*srcfs, *dstfs);

	copier.onError([](IFS::FileSystem& fileSys, int errorCode, IFS::FileCopier::Operation operation,
					  const String& path) -> bool { return true; });

	bool res = copier.copyDir(nullptr, nullptr);
	dstfs->setProfiler(nullptr);

	IFS::FileSystem::Info srcinfo;
	srcfs->getinfo(srcinfo);
	IFS::FileSystem::Info dstinfo;
	dstfs->getinfo(dstinfo);

	delete srcfs;

	auto kb = [](volume_size_t size) { return (size + 1023) / 1024; };

	Serial << "Source " << srcinfo.type << " size: " << kb(srcinfo.used()) << " KB; Output " << dstinfo.type
		   << " used: " << kb(dstinfo.used()) << " KB, free: " << kb(dstinfo.freeSpace) << " KB" << endl;

	Serial << "Perf stats: " << profiler << endl;

	return res;
}

void fsinit()
{
	DEFINE_FSTR(test_image, "/mnt/c/temp/2017-07-05-raspbian-jessie-lite.img")
	auto dev = mountTestImage("TEST", test_image);

	Storage::Debug::listPartitions(Serial);
	return;

	DEFINE_FSTR_LOCAL(newfile_txt, "The name of this file is, perhaps, a little long.txt");

	auto part = Storage::findDefaultPartition(Storage::Partition::SubType::Data::fat);
	auto fs = IFS::createFatFilesystem(part);

	int err = fs->mount();
	if(err == FS_OK) {
		fileSetFileSystem(fs);
	} else if(err == IFS::Error::BadFileSystem) {
		debug_i("Formatting disk");
		fs->format();
		fileMountFileSystem(fs);
		fscopy("fwfs1.bin");

		int err = fileSetContent(newfile_txt, F("It works!\r\n"));
		debug_i("fileSetContent(): %s", fileGetErrorString(err).c_str());
	} else {
		debug_e("Unhandled error during mount: %s", fs->getErrorString(err).c_str());
		return;
	}

	String s = fileGetContent(newfile_txt);

	m_puts(_F("Read: "));
	m_nputs(s.c_str(), s.length());
	m_puts("\r\n");

	IFS::Debug::printFsInfo(Serial, *fs);

	IFS::Debug::listDirectory(Serial, *fs, nullptr, IFS::Debug::Option::recurse);
}

} // namespace

void init()
{
#if DEBUG_BUILD
	Serial.begin(COM_SPEED_SERIAL);

	Serial.systemDebugOutput(true);
	debug_i("\n\n********************************************************\n"
			"Hello\n");
#endif

	fsinit();
}
