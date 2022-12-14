#include <SmingTest.h>
#include <IFS/FAT.h>
#include <IFS/Debug.h>
#include <IFS/FileCopier.h>
#include <Storage/Disk.h>
#include <Storage/SD/Card.h>
#include <SPI.h>
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

using namespace Storage;

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

#ifdef ARCH_HOST
class HostCard : public Disk::HostFileDevice
{
public:
	HostCard(const String& name, SPIBase& spi) : HostFileDevice(name, "out/sdcard.img", 4000000000)
	{
		const uint8_t csd_data[]{0x40, 0x0e, 0x00, 0x32, 0x5b, 0x59, 0x00, 0x00,
								 0xee, 0x7f, 0x7f, 0x80, 0x0a, 0x40, 0x40, 0x55};
		static_assert(sizeof(csd_data) == sizeof(csd));
		memcpy(&csd, csd_data, sizeof(csd_data));
		csd.bswap();

		const uint8_t cid_data[]{0x1b, 0x53, 0x4d, 0x45, 0x42, 0x31, 0x51, 0x54,
								 0x30, 0xf1, 0x77, 0x5f, 0xea, 0x01, 0x1a, 0xb9};
		static_assert(sizeof(cid_data) == sizeof(cid));
		memcpy(&cid, cid_data, sizeof(cid));
		cid.bswap();
	}

	bool begin(uint8_t)
	{
		return getSize() != 0;
	}

	SD::CSD csd;
	SD::CID cid;
};
#endif

} // namespace

class SdCardTest : public TestGroup
{
public:
	SdCardTest() : TestGroup(_F("SdCard")), card("card1", SPI)
	{
		Storage::registerDevice(&card);
		// Enable buffering to support byte read/write
		REQUIRE(card.allocateBuffers(4));
	}

	void execute() override
	{
		REQUIRE(card.begin(PIN_CARD_CS));

		REQUIRE(fwfs_mount());

		Serial << "CSD" << endl << card.csd << endl;
		Serial << "CID" << endl << card.cid;
		for(auto part : card.partitions()) {
			Serial << part << endl;
		}

		TEST_CASE("Format card")
		{
			// Split disk into equal partitions
			Disk::GPT::PartitionTable table;
			table.add(F("gpt1-fwfs"), Partition::SubType::Data::fwfs, 0, 100 / 8);
			table.add(F("gpt2-spiffs"), Partition::SubType::Data::spiffs, 0, 100 / 8);
			table.add(F("gpt3-littlefs"), Partition::SubType::Data::littlefs, 0, 100 / 8);
			table.add(F("gpt4-fat"), Partition::SubType::Data::fat, 0, 100 / 8);
			auto err = Disk::formatDisk(card, table);
			Serial << "formatDisk: " << err << endl;

			auto part = *card.partitions().find(Partition::SubType::Data::fat);
			int fmterr = IFS::FAT::formatVolume(part);
			Serial << "formatVolume " << IFS::Error::toString(fmterr) << endl;
			REQUIRE(fmterr == 0);
		}

		TEST_CASE("Copy files to LittleFS")
		{
			auto part = Storage::findDefaultPartition(Partition::SubType::Data::littlefs);
			auto fs = mountVolume(part);
			FS_CHECK(copyFiles(*fs));
			Serial << card.stat << endl;
			card.stat = {};
			delete fs;
		}

		TEST_CASE("Copy files to FAT")
		{
			auto part = Storage::findDefaultPartition(Partition::SubType::Data::fat);
			auto fs = mountVolume(part);
			FS_CHECK(copyFiles(*fs));
			Serial << card.stat << endl;
			card.stat = {};
			delete fs;
		}

		TEST_CASE("Re-scan disk")
		{
			Disk::scanPartitions(card);
			for(auto part : card.partitions()) {
				Serial << part << endl;
			}
		}

		TEST_CASE("Verify LittleFS")
		{
			auto part = Storage::findDefaultPartition(Partition::SubType::Data::littlefs);
			auto fs = mountVolume(part);
			verifyFiles(*fs);
			Serial << "Files verified." << endl;
			Serial << card.stat << endl;
			card.stat = {};
			delete fs;
		}

		TEST_CASE("Verify FAT")
		{
			auto part = Storage::findDefaultPartition(Partition::SubType::Data::fat);
			auto fs = mountVolume(part);
			verifyFiles(*fs);
			Serial << "Files verified." << endl;
			Serial << card.stat << endl;
			card.stat = {};
			delete fs;
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
			if(abs(srcStat.mtime - dstStat.mtime) >= 2) {
				Serial << "srctime " << srcStat.mtime.toString() << ", dsttime " << dstStat.mtime.toString() << endl;
			}
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
		IFS::FileSystem* fs;
		switch(part.fullType().value()) {
		case Partition::FullType(Partition::SubType::Data::spiffs).value():
			fs = IFS::createSpiffsFilesystem(part);
			break;
		case Partition::FullType(Partition::SubType::Data::littlefs).value():
			fs = IFS::createLfsFilesystem(part);
			break;
		case Partition::FullType(Partition::SubType::Data::fat).value():
			fs = IFS::createFatFilesystem(part);
			break;
		default:
			Serial << "Bad partition: " << part << endl;
			TEST_ASSERT(false);
		}

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
#ifdef ARCH_HOST
	HostCard card;
#else
	SD::Card card;
#endif
};

void REGISTER_TEST(sdcard)
{
	registerGroup<SdCardTest>();
}
