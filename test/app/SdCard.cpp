#include <SmingTest.h>
#include <IFS/FAT.h>
#include <IFS/Debug.h>
#include <IFS/FileCopier.h>
#include <Storage/Disk.h>
#include <Storage/SD/Card.h>
#include <Storage/CustomDevice.h>
#include <SPI.h>
#include <Storage/Disk/BufferedDevice.h>
#include <LittleFS.h>

// Chip selects independent of SPI controller in use
#ifdef ARCH_ESP32
#define PIN_CARD_CS 21
#else
// Esp8266 cannot use GPIO15 as this affects boot mode
#define PIN_CARD_CS 5
#endif

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

class SdCardTest : public TestGroup
{
public:
	SdCardTest() : TestGroup(_F("SdCard")), sdcard("card1", SPI), card(sdcard, "card1b", 4)
	{
		Storage::registerDevice(&sdcard);
		Storage::registerDevice(&card);
	}

	void execute() override
	{
		REQUIRE(sdcard.begin(PIN_CARD_CS));

		REQUIRE(fwfs_mount());

		Serial << "CSD" << endl << sdcard.csd << endl;
		Serial << "CID" << endl << sdcard.cid;
		for(auto part : card.partitions()) {
			Serial << part << endl;
		}

		TEST_CASE("Format card")
		{
			// Split disk into equal partitions
			Disk::GPT::PartitionTable table;
			table.add(String("gpt1"), Partition::SubType::Data::littlefs, 0, 100 / 8);
			for(unsigned i = 1; i < 8; ++i) {
				table.add(String("gpt") + String(i + 1), Disk::SysType::exfat, 0, 100 / 8);
			}
			auto err = Disk::formatDisk(card, table);
			Serial << "formatDisk: " << err << endl;

			// // Format the first partition using FAT
			// auto part = *card.partitions().begin();
			// int fmterr = IFS::FAT::formatVolume(part);
			// Serial << "formatVolume " << IFS::Error::toString(fmterr) << endl;
			// REQUIRE(fmterr == 0);

			// Format the first partition using LittleFS
			auto part = *card.partitions().begin();
			auto fs = IFS::createLfsFilesystem(part);
			int fmterr = fs->format();
			delete fs;
			Serial << "formatVolume " << IFS::Error::toString(fmterr) << endl;
			REQUIRE(fmterr == 0);

			// Now mount the volume
			fs = mountVolume(part);
			FS_CHECK(copyFiles(*fs));
			delete fs;

			Serial << card.stat << endl;
			card.stat = {};
		}

		TEST_CASE("Read volume", "Re-open volume then verify all written files")
		{
			Disk::scanPartitions(card);
			auto part = *card.partitions().begin();

			for(auto part : card.partitions()) {
				Serial << part << endl;
			}
			auto fs = mountVolume(part);

			verifyFiles(*fs);
			Serial << "Files verified." << endl;

			delete fs;

			Serial << card.stat << endl;
			card.stat = {};
		}
	}

	int copyFiles(IFS::FileSystem& fs, const String& srcPath = nullptr)
	{
		auto& hfs = *IFS::getDefaultFileSystem();
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

	void verifyFiles(IFS::FileSystem& fs, const String& srcPath = nullptr, const String& dstPath = nullptr)
	{
		auto& hfs = *IFS::getDefaultFileSystem();

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
				TEST_ASSERT(false);
				return;
			}
			File dstFile(&fs);
			if(!dstFile.open(dstFileName)) {
				handleError({dstFile, IFS::FileCopier::Operation::open, dstFileName});
				TEST_ASSERT(false);
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

	IFS::FileSystem* mountVolume(Partition part)
	{
		auto fs = IFS::createLfsFilesystem(part);
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
	SD::Card sdcard;
	Disk::BufferedDevice card;
};

void REGISTER_TEST(sdcard)
{
	registerGroup<SdCardTest>();
}
