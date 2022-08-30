#include <SmingCore.h>

#include <IFS/FatFS.h>
#include <IFS/FileCopier.h>
#include <Data/Stream/IFS/DirectoryTemplate.h>
#include <Data/Stream/MemoryDataStream.h>
#include <Data/CStringArray.h>

namespace
{
#define FF_CHECK(func, err) debug_i(func "(): %s", fileGetErrorString(err).c_str())
IMPORT_FSTR(listing_txt, PROJECT_DIR "/resource/listing.txt")

bool fscopy(const char* srcFile)
{
	auto& hostfs = IFS::Host::getFileSystem();

	auto srcfs = IFS::mountArchive(hostfs, srcFile);
	if(srcfs == nullptr) {
		m_printf("mount failed: %s", srcFile);
		return false;
	}

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

void printDirectory(const String& path)
{
	auto dir = new Directory;
	if(!dir->open(path)) {
		debug_e("Open '%s' failed: %s", path.c_str(), dir->getLastErrorString().c_str());
		delete dir;
		return;
	}

	CStringArray paths;

	{
		auto source = new FlashMemoryStream(listing_txt);
		IFS::DirectoryTemplate tmpl(source, dir);
		Serial.copyFrom(&tmpl);

		dir->rewind();
		while(dir->next()) {
			if(!dir->stat().isDir()) {
				continue;
			}
			paths += dir->stat().name;
		}
	}

	for(auto sub : paths) {
		String s;
		if(path.length() != 0) {
			s += path;
			s += '/';
		}
		s += sub;
		printDirectory(s);
	}
}

void fsinit()
{
	int err = fatfs_mount();
	FF_CHECK("mount", err);

	bool createVolume = true;
	bool writeFile = createVolume;

	if(createVolume) {
		debug_i("Formatting disk");
		int err = fileSystemFormat();
		FF_CHECK("format", err);

		fscopy("fwfs1.bin");
	}

	DEFINE_FSTR_LOCAL(newfile_txt, "The name of this file is, perhaps, a little long.txt");

	if(writeFile) {
		int err = fileSetContent(newfile_txt, F("It works!\r\n"));
		FF_CHECK("fileSetContent", err);
	}

	String s = fileGetContent(newfile_txt);

	m_puts(_F("Read: "));
	m_nputs(s.c_str(), s.length());
	m_puts("\r\n");

	printDirectory("");
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
