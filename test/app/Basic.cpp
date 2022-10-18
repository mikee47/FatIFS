#include <SmingTest.h>
#include <IFS/FAT.h>
#include <IFS/Debug.h>
#include <IFS/FileCopier.h>
#include <Storage/Disk.h>

#define NUM_BUFFERED_DEVICE_SECTORS 4

#ifdef ARCH_HOST
#include <Storage/HostFileDevice.h>
#else
#include <Storage/CustomDevice.h>
#endif

#define DIV_KB 1024ULL
#define DIV_MB (DIV_KB * DIV_KB)
#define DIV_GB (DIV_KB * DIV_MB)

#define FS_CHECK(a)                                                                                                    \
	{                                                                                                                  \
		int res = a;                                                                                                   \
		Serial << #a << " : " << fs->getErrorString(res) << endl;                                                      \
		TEST_ASSERT(res >= 0);                                                                                         \
	}

namespace
{
// TODO: Add `Path` class to IFS for manipulating filenames
String abspath(const String& path, const char* name)
{
	String s;
	if(path.length() != 0) {
		s += path;
		s += '/';
	}
	return s + name;
}
} // namespace

using namespace Storage;

class BasicTest : public TestGroup
{
public:
	BasicTest() : TestGroup(_F("Basic"))
	{
	}

	void execute() override
	{
		DEFINE_FSTR_LOCAL(DEVICE1_FILENAME, "out/test1.img")
		DEFINE_FSTR_LOCAL(DEVICE2_FILENAME, "out/test2.img")

#ifdef ENABLE_EXFAT
		const storage_size_t devSize = 128 * DIV_GB;
		Disk::SysTypes sysTypes = Disk::SysType::exfat;
#elif defined(ENABLE_STORAGE_SIZE64)
		const storage_size_t devSize = 8 * DIV_GB;
		Disk::SysTypes sysTypes = Disk::SysType::fat32;
#else
		const storage_size_t devSize = INT32_MAX - 512;
		Disk::SysTypes sysTypes = Disk::SysType::fat32;
#endif

		String srcPath = String(getenv("SMING_HOME")) + "/out";

		TEST_CASE("Create volume", "Create maximal volume and populate")
		{
			auto dev = createDevice(DEVICE1_FILENAME, devSize);
			auto part = addPartition(dev);

			formatVolume(part, sysTypes);
			auto fs = mountVolume(part);
			FS_CHECK(copyFiles(*fs, srcPath));
			Serial << dev->stat << endl;

			delete fs;
			destroyDevice();
		}

		TEST_CASE("Read volume", "Re-open volume then verify all written files")
		{
			auto dev = openDevice(DEVICE1_FILENAME);
			auto part = addPartition(dev);
			auto fs = mountVolume(part);

			verifyFiles(*fs, srcPath);
			Serial << "Files verified." << endl;
			Serial << dev->stat << endl;

			delete fs;
			destroyDevice();
		}

		TEST_CASE("fatfs forum 3682", "Check handling of re-writing files")
		{
			auto dev = openDevice(DEVICE1_FILENAME);
			auto part = addPartition(dev);
			auto fs = mountVolume(part);

			auto filename = "test.txt";

			IFS::File file(fs);
			CHECK(file.open(filename, File::CreateNewAlways | File::ReadWrite));
			CHECK_EQ(file.write(F("TOTO :)")), 6);
			CHECK_EQ(file.seek(0, SeekOrigin::Start), 0);
			CHECK_EQ(file.write(F("TEST")), 4);
			CHECK(file.truncate());
			CHECK_EQ(file.seek(0, SeekOrigin::Start), 0);
			REQUIRE_EQ(file.getContent(), "TEST");
			CHECK(file.close());

			CHECK(file.open(filename, File::ReadWrite));
			CHECK_EQ(file.write(F("TOTO :)")), 7);
			CHECK(file.close());

			CHECK(file.open(filename, File::ReadWrite));
			REQUIRE_EQ(file.getContent(), "TOTO :)");
			CHECK_EQ(file.seek(0, SeekOrigin::Start), 0);
			CHECK_EQ(file.write(F("TEST")), 4);
			CHECK(file.close());

			CHECK(file.open(filename, File::ReadWrite));
			REQUIRE_EQ(file.getContent(), "TEST :)");
			CHECK_EQ(file.seek(0, SeekOrigin::Start), 0);
			CHECK_EQ(file.write(F("TEST")), 4);
			CHECK(file.truncate());
			CHECK(file.close());

			CHECK(file.open(filename, File::ReadWrite));
			REQUIRE(file.getContent() == "TEST");
			CHECK(file.close());

			Serial << dev->stat << endl;

			delete fs;
			destroyDevice();
		}

		TEST_CASE("fatfs forum 3608", "Check creation, renaming and removal of directories")
		{
			auto dev = openDevice(DEVICE1_FILENAME);
			auto part = addPartition(dev);
			auto fs = mountVolume(part);

			DEFINE_FSTR_LOCAL(FS_dir1, "TestDir")
			DEFINE_FSTR_LOCAL(FS_dir2, "Test Directory")
			DEFINE_FSTR_LOCAL(FS_dir3, "Test Directory Number Two")
			fs->remove(FS_dir1);
			fs->remove(FS_dir2);
			fs->remove(FS_dir3);

			REQUIRE_EQ(fs->stat(FS_dir1, nullptr), IFS::Error::NotFound);

			FS_CHECK(fs->mkdir(FS_dir1));
			FS_CHECK(fs->stat(FS_dir1, nullptr));

			FS_CHECK(fs->mkdir(FS_dir2));
			FS_CHECK(fs->stat(FS_dir2, nullptr));
			FS_CHECK(fs->rename(FS_dir2, FS_dir3));
			REQUIRE_EQ(fs->stat(FS_dir2, nullptr), IFS::Error::NotFound);
			FS_CHECK(fs->stat(FS_dir3, nullptr));

			Serial << dev->stat << endl;

			delete fs;
			destroyDevice();
		}

		TEST_CASE("fatfs forum 3628", "Check creation and removal of files")
		{
			auto dev = openDevice(DEVICE1_FILENAME);
			auto part = addPartition(dev);
			auto fs = mountVolume(part);

			FileHandle f;
			FS_CHECK((f = fs->open("test.dat", File::CreateNewAlways | File::WriteOnly)));
			FS_CHECK(fs->write(f, "test", 4));
			FS_CHECK(fs->close(f));
			FS_CHECK((f = fs->open("test.dat")));
			FS_CHECK(fs->remove("test.dat"));
			REQUIRE_EQ(fs->open("test.dat"), IFS::Error::NotFound);

			Serial << dev->stat << endl;

			delete fs;
			destroyDevice();
		}

		TEST_CASE("Full volume", "Writes must fail when volume is full")
		{
			auto dev = createDevice("out/temp.img", 10 * DIV_MB);
			auto part = addPartition(dev);
			formatVolume(part, Disk::SysType::fat16);
			auto fs = mountVolume(part);
			REQUIRE_EQ(copyFiles(*fs, srcPath), IFS::Error::NoSpace);
			Serial << dev->stat << endl;

			delete fs;
			destroyDevice();
		}

#if defined(ENABLE_STORAGE_SIZE64) && !defined(ENABLE_FILE_SIZE64)
		TEST_CASE("File too large", "Writes must fail when file gets too big")
		{
			auto dev = createDevice(DEVICE1_FILENAME, 5 * DIV_GB);
			auto part = addPartition(dev);
			formatVolume(part, Disk::SysType::fat32);
			auto fs = mountVolume(part);

			FileHandle file;
			FS_CHECK((file = fs->open("test.dat", File::CreateNewAlways | File::WriteOnly)));
			FS_CHECK(fs->ftruncate(file, 3.9 * DIV_GB));
			Serial << "pos = " << fs->tell(file) << endl;
			uint64_t count = fs->getSize(file);
			uint8_t buf[1024];
			os_get_random(buf, sizeof(buf));
			for(;;) {
				int res = fs->write(file, buf, sizeof(buf));
				if(res != sizeof(buf)) {
					Serial << count << " bytes written, " << res << ": " << fs->getErrorString(res) << endl;
					REQUIRE_EQ(res, IFS::Error::TooBig);
					break;
				}
				count += sizeof(buf);
			}
			FS_CHECK(fs->close(file));

			Serial << dev->stat << endl;

			delete fs;
			destroyDevice();
		}

		TEST_CASE("Large seek", "Seeking past 2GB must fail")
		{
			auto dev = createDevice(DEVICE1_FILENAME, 5 * DIV_GB);
			auto part = addPartition(dev);
			formatVolume(part, Disk::SysType::fat32);
			auto fs = mountVolume(part);

			FileHandle file;
			FS_CHECK((file = fs->open("test.dat", File::CreateNewAlways | File::WriteOnly)));
			FS_CHECK(fs->ftruncate(file, 3.9 * DIV_GB));
			REQUIRE_EQ(fs->tell(file), IFS::Error::TooBig);
			REQUIRE_EQ(fs->lseek(file, 2 * DIV_GB, SeekOrigin::Start), IFS::Error::SeekBounds);
			FS_CHECK(fs->ftruncate(file, 2 * DIV_GB - 1));
			REQUIRE_EQ(fs->tell(file), 2 * DIV_GB - 1);
			FS_CHECK(fs->close(file));

			Serial << dev->stat << endl;

			delete fs;
			destroyDevice();
		}
#endif

		TEST_CASE("Partition disk")
		{
			auto dev = createDevice(DEVICE2_FILENAME, 4 * DIV_GB - 512);

			Disk::GPT::PartitionTable table;
			table.add("My FAT partition", Disk::SysType::fat32, 0, 50);
			table.add("My other partition", Disk::SysType::fat16, 0, 10);
			table.add("yet another one", Disk::SysType::fat16, 0, 10);
			table.add("hello mr blobby", Disk::SysType::fat16, 0, 10);
			auto err = Disk::formatDisk(*dev, table);
			Serial << "formatDisk: " << err << endl;

			IFS::FAT::FormatOptions opt{
#ifdef ENABLE_EXFAT
				.types = Disk::SysType::exfat,
#endif
			};
			for(auto part : dev->partitions()) {
				Serial << part << endl;
				CHECK(part.diskpart() != nullptr);
				opt.volumeLabel = F("FAT_") + os_random() % 10000;
				REQUIRE(IFS::FAT::formatVolume(part, opt) == FS_OK);
				opt.types = 0;
			}

			for(auto part : dev->partitions()) {
				Serial << *part.diskpart() << endl;
				CHECK(part.diskpart() != nullptr);
				auto fs = IFS::createFatFilesystem(part);
				REQUIRE(fs != nullptr);
				int err = fs->mount();
				REQUIRE(err == FS_OK);
				DEFINE_FSTR_LOCAL(filename, "test-file-for-partition.tst")
				DEFINE_FSTR_LOCAL(FS_test, "# This is a test file. It doesn't do much. #")
				REQUIRE(fs->setContent(filename, FS_test));
				REQUIRE_EQ(fs->getContent(filename), FS_test);
				IFS::Debug::listDirectory(Serial, *fs, nullptr);
				delete fs;
			}

			Serial << dev->stat << endl;

			destroyDevice();
		}
	}

	int copyFiles(IFS::FileSystem& fs, const String& srcPath)
	{
#ifdef ARCH_HOST
		auto& hfs = IFS::Host::getFileSystem();
#else
		auto& hfs = *IFS::getDefaultFileSystem();
#endif
		IFS::FileCopier copier(hfs, fs);

		int err{FS_OK};

		copier.onError([&](const IFS::FileCopier::ErrorInfo& info) -> bool {
			/*
			 * FAT doesn't have a root directory entry, so cannot be opened with write access.
			 * (Read access is faked.)
			 */
			if(info.operation == IFS::FileCopier::Operation::open && !info.path) {
				return true;
			}
			// FAT doesn't support custom attributes, so ignore these errors
			if(info.operation == IFS::FileCopier::Operation::setattr) {
				return true;
			}
			Serial << info << endl;
			err = info.errorCode;
			return false;
		});

		copier.copyDir(srcPath, nullptr);

		IFS::FileSystem::Info dstinfo;
		fs.getinfo(dstinfo);

		auto kb = [](volume_size_t size) { return (size + 1023) / 1024; };

		Serial << "Output " << dstinfo.type << " used: " << kb(dstinfo.used()) << " KB, free: " << kb(dstinfo.freeSpace)
			   << " KB" << endl;

		return err;
	}

	void handleError(const IFS::FileCopier::ErrorInfo& info)
	{
		Serial << info << endl;
	}

	void verifyFiles(IFS::FileSystem& fs, const String& srcPath, const String& dstPath = nullptr)
	{
#ifdef ARCH_HOST
		auto& hfs = IFS::Host::getFileSystem();
#else
		auto& hfs = *IFS::getDefaultFileSystem();
#endif

		Directory srcDir(&hfs);
		if(!srcDir.open(srcPath)) {
			Serial << "open(\"" << srcPath << "\"): " << srcDir.getLastErrorString() << endl;
			TEST_ASSERT(false);
		}

		struct Dir {
			CString name;
			IFS::TimeStamp mtime;
		};
		Vector<Dir> directories;

		while(srcDir.next()) {
			auto& stat = srcDir.stat();
			if(stat.isDir()) {
				// Skip mount points
				if(!stat.attr[FileAttribute::MountPoint]) {
					directories.add({stat.name.c_str(), stat.mtime});
				}
				continue;
			}

			String srcFileName = abspath(srcPath, stat.name.c_str());
			String dstFileName = abspath(dstPath, stat.name.c_str());

			File srcFile(&hfs);
			if(!srcFile.open(srcFileName)) {
				handleError({srcFile, IFS::FileCopier::Operation::open, srcFileName});
				return;
			}
			File dstFile(&fs);
			if(!dstFile.open(dstFileName)) {
				handleError({dstFile, IFS::FileCopier::Operation::open, dstFileName});
				return;
			}
			srcFile.readContent([&dstFile](const char* buffer, size_t size) -> int {
				char dstbuf[size];
				if(!dstFile.read(dstbuf, size)) {
					return dstFile.getLastError();
				}
				return (memcmp(buffer, dstbuf, size) == 0) ? FS_OK : IFS::Error::BadObject;
			});

			IFS::Stat srcStat;
			srcFile.stat(srcStat);
			IFS::Stat dstStat;
			dstFile.stat(dstStat);
			CHECK(abs(srcStat.mtime - dstStat.mtime) < 2);
			CHECK_EQ(srcStat.size, dstStat.size);
			CHECK_EQ(srcStat.attr, dstStat.attr);
		}

		srcDir.close();

		for(auto& dir : directories) {
			String srcDirPath = abspath(srcPath, dir.name.c_str());
			String dstDirPath = abspath(dstPath, dir.name.c_str());
			verifyFiles(fs, srcDirPath, dstDirPath);
		}
	}

	Disk::BufferedDevice* createDevice(const String& filename, storage_size_t size)
	{
		CHECK(bufferedDevice == nullptr);
		CHECK(device == nullptr);
#ifdef ARCH_HOST
		device = new Storage::HostFileDevice("test", filename, size);
#endif
		if(device == nullptr || device->getSize() == 0) {
			Serial << _F("Failed to create '") << filename << ": " << endl;
			TEST_ASSERT(false);
		}

		Serial << "Created \"" << filename << "\", " << device->getSize() << " bytes." << endl;

		REQUIRE(registerDevice(device));

		bufferedDevice = new Storage::Disk::BufferedDevice(*device, "test-buffered", NUM_BUFFERED_DEVICE_SECTORS);
		REQUIRE(registerDevice(bufferedDevice));

		return bufferedDevice;
	}

	Disk::BufferedDevice* openDevice(const String& filename)
	{
		CHECK(bufferedDevice == nullptr);
		CHECK(device == nullptr);
#ifdef ARCH_HOST
		device = new Storage::HostFileDevice("test", filename);
#endif
		if(device == nullptr || device->getSize() == 0) {
			Serial << _F("Failed to open '") << filename << ": " << endl;
			TEST_ASSERT(false);
		}

		Serial << "Opened \"" << filename << "\", " << device->getSize() << " bytes." << endl;

		REQUIRE(registerDevice(device));

		bufferedDevice = new Storage::Disk::BufferedDevice(*device, "test-buffered", NUM_BUFFERED_DEVICE_SECTORS);
		REQUIRE(registerDevice(bufferedDevice));

		return bufferedDevice;
	}

	void destroyDevice()
	{
		CHECK(bufferedDevice != nullptr);
		delete bufferedDevice;
		bufferedDevice = nullptr;

		CHECK(device != nullptr);
		delete device;
		device = nullptr;
	}

	Partition addPartition(Device* dev)
	{
		auto& pt = static_cast<CustomDevice*>(dev)->partitions();
		auto part = pt.add("TEST", Partition::SubType::Data::fat, 0, dev->getSize());
		Serial << part << endl;
		REQUIRE(part);
		return part;
	}

	void formatVolume(Partition part, Disk::SysTypes sysTypes)
	{
		IFS::FAT::FormatOptions opt{
			.types = sysTypes,
		};
		int err = IFS::FAT::formatVolume(part, opt);
		Serial << "formatVolume " << IFS::Error::toString(err) << endl;
		REQUIRE(err == 0);
	}

	IFS::FileSystem* mountVolume(Partition part)
	{
		auto fs = IFS::createFatFilesystem(part);
		int err = fs->mount();
		debug_i("mount: %s", fs->getErrorString(err).c_str());
		REQUIRE(err == 0);

		Serial.println(_F("FS info:"));
		IFS::Debug::printFsInfo(Serial, *fs);
#if DEBUG_VERBOSE_LEVEL == DBG
		IFS::Debug::listDirectory(Serial, *fs, nullptr, IFS::Debug::Option::recurse);
#endif

		return fs;
	}

private:
	Device* device{nullptr};
	Disk::BufferedDevice* bufferedDevice{nullptr};
};

void REGISTER_TEST(basic)
{
	registerGroup<BasicTest>();
}
