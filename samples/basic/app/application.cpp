#include <SmingCore.h>
#include <IFS/FAT.h>
#include <IFS/FileCopier.h>
#include <IFS/Debug.h>
#include <IFS/FileSystem.h>
#include <Storage/FileDevice.h>
#include <Storage/DiskDevice.h>
#include <Storage/DiskScanner.h>
#include <Storage/Debug.h>
#include <Storage/Sdio.h>
#include <IFS/Enumerator.h>
#include <IFS/FAT/Format.h>

// Chip selects independent of SPI controller in use
#ifdef ARCH_ESP32
#define PIN_CARD_CS 21
#else
// Esp8266 cannot use GPIO15 as this affects boot mode
#define PIN_CARD_CS 5
#endif

#define SPI_FREQ_LIMIT 0 //2000000

namespace
{
void enumCallback(void* param)
{
	auto e = static_cast<IFS::Debug::Enumerator*>(param);
	if(e->next()) {
		System.queueCallback(enumCallback, e);
	} else {
		Serial.println("*** DONE ***");
		delete e;
	}
}

void listDirectoryAsync(IFS::FileSystem* fs, const String& path)
{
	auto e = new IFS::Debug::Enumerator(fs, path);
	e->onStat([](const String& path, const IFS::Stat& stat) {
		String s = path + '/' + stat.name;
		constexpr size_t maxWidth{80};
		if(s.length() >= maxWidth) {
			Serial.println(s);
			Serial.print(String().pad(maxWidth));
		} else {
			Serial.print(s.pad(maxWidth));
		}
		Serial << String(stat.size, DEC).padLeft(8) << ' ' << String(stat.mtime) << ' '
			   << getFileAttributeString(stat.attr) << endl;

		// Serial << stat << endl;
	});

	enumCallback(e);
}

bool fscopy(Storage::Partition fwfsPart, IFS::FileSystem& dstfs)
{
	auto srcfs = IFS::createFirmwareFilesystem(fwfsPart);
	if(srcfs == nullptr) {
		debug_e("Source FWFS filesystem not set");
		return false;
	}
	srcfs->mount();

	IFS::Profiler profiler;
	dstfs.setProfiler(&profiler);
	IFS::FileCopier copier(*srcfs, dstfs);

	copier.onError([](const auto& info) -> bool {
		if(info.operation != IFS::FileCopier::Operation::setattr) {
			Serial << info << endl;
		}
		return true;
	});

	bool res = copier.copyDir(nullptr, nullptr);
	dstfs.setProfiler(nullptr);

	IFS::FileSystem::Info srcinfo;
	srcfs->getinfo(srcinfo);
	IFS::FileSystem::Info dstinfo;
	dstfs.getinfo(dstinfo);

	delete srcfs;

	auto kb = [](volume_size_t size) { return (size + 1023) / 1024; };

	Serial << "Source " << srcinfo.type << " size: " << kb(srcinfo.used()) << " KB; Output " << dstinfo.type
		   << " used: " << kb(dstinfo.used()) << " KB, free: " << kb(dstinfo.freeSpace) << " KB" << endl;

	Serial << "Perf stats: " << profiler << endl;

	return res;
}

#ifdef ARCH_HOST
void mountTestImage(const String& tag, const String& filename)
{
	auto& hfs = IFS::Host::getFileSystem();
	auto f = hfs.open(filename, IFS::File::ReadOnly);
	if(f < 0) {
		Serial << _F("Failed to open '") << filename << ": " << hfs.getErrorString(f) << endl;
		return;
	}

	Serial << _F("Mounted '") << filename << '\'' << endl;

	auto dev = new Storage::FileDevice(tag, hfs, f);
	Storage::registerDevice(dev);

	Storage::scanDiskPartitions(*dev);

	for(auto part : Storage::findPartition()) {
		Serial << _F("Disk Partition:") << endl << Storage::DiskPart(part) << endl;
	}

	for(auto part : dev->partitions()) {
		auto fs = IFS::createFatFilesystem(part);
		int err = fs->mount();
		debug_i("mount: %s", fs->getErrorString(err).c_str());
		if(err == FS_OK) {
			Serial.println(_F("FS info:"));
			IFS::Debug::printFsInfo(Serial, *fs);
			IFS::Debug::listDirectory(Serial, *fs, nullptr, IFS::Debug::Option::recurse);

			// listDirectoryAsync(fs, nullptr);
			// return;

			FileStream file(fs);
			file.open(F("Sming/README.rst"));
			Serial.copyFrom(&file);
		}
		delete fs;

		Serial.println();
	}

	delete dev;

	Storage::Debug::listPartitions(Serial);
}

void createTestImage(const String& tag, const String& filename)
{
	auto& hfs = IFS::Host::getFileSystem();

	auto f = hfs.open(filename, File::ReadWrite);
	bool create = (f < 0);
	if(create) {
		f = hfs.open(filename, File::CreateNewAlways | File::ReadWrite);
		if(f < 0) {
			Serial << _F("Failed to open '") << filename << ": " << hfs.getErrorString(f) << endl;
			return;
		}
		hfs.ftruncate(f, 100 * 1024 * 1024);
	}

	Serial << _F("Mounted '") << filename << '\'' << endl;

	auto dev = new Storage::FileDevice(tag, hfs, f);
	Storage::registerDevice(dev);

	if(create) {
#if 0
		Storage::MBR::PartitionSpec spec{
			{
				.size = 100,
				.name = "test partition",
			},
			.sysIndicator = Storage::DiskPart::SysIndicator::SI_FAT16,
		};
		auto err = Storage::MBR::createPartition(*dev, &spec, 1);
#else
		Storage::GPT::PartitionSpec spec{{
			.size = 100,
			.name = "test partition",
		}};
		auto err = Storage::GPT::createPartition(*dev, &spec, 1);
#endif
		Serial << "createPartition " << IFS::Error::toString(err) << endl;

		Storage::scanDiskPartitions(*dev);

		auto part = *dev->partitions().begin();

		IFS::FAT::MKFS_PARM opt{
			// .types = Storage::DiskPart::SysType::exfat,
		};
		err = IFS::FAT::formatVolume(part, opt);
		Serial << "formatVolume " << IFS::Error::toString(err) << endl;
	}

	Storage::scanDiskPartitions(*dev);

	for(auto part : Storage::findPartition()) {
		Serial << _F("Disk Partition:") << endl << Storage::DiskPart(part) << endl;
	}

	for(auto part : dev->partitions()) {
		auto fs = IFS::createFatFilesystem(part);
		int err = fs->mount();
		debug_i("mount: %s", fs->getErrorString(err).c_str());
		if(err == FS_OK) {
			if(create) {
				auto part = Storage::findDefaultPartition(Storage::Partition::SubType::Data::fwfs);
				fscopy(part, *fs);
			}

			Serial.println(_F("FS info:"));
			IFS::Debug::printFsInfo(Serial, *fs);
			IFS::Debug::listDirectory(Serial, *fs, nullptr); //, IFS::Debug::Option::recurse);

			// listDirectoryAsync(fs, nullptr);
			// return;

			FileStream file(fs);
			file.open(F("Sming/README.rst"));
			Serial.copyFrom(&file);

			fs->setContent("test.txt", F("This is a test file.\r\n"));
		}
		delete fs;

		Serial.println();
	}

	delete dev;

	Storage::Debug::listPartitions(Serial);
}
#endif

Storage::Partition sdinit()
{
	auto card = new Storage::SDIO::Card(SPI);
	Storage::registerDevice(card);

	if(!card->begin(PIN_CARD_CS, SPI_FREQ_LIMIT)) {
		return Storage::Partition{};
	}

	Storage::SDIO::CardID cid;
	if(card->read_cid(cid)) {
		m_printHex("CID", &cid, sizeof(cid));
		Serial << "Card Identification Information" << endl << cid;
	}

	for(auto part : card->partitions()) {
		Serial << _F("Disk Partition:") << endl << Storage::DiskPart(part) << endl;
	}

	return *card->partitions().find(Storage::Partition::SubType::Data::fat);
}

void fsinit()
{
#ifdef ARCH_HOST
	// mountTestImage("TEST", "test");
	// mountTestImage("TEST2", "test2");
	// return;

	createTestImage("GENTEST", "gentest.img");

#endif

	auto part = sdinit();
#ifdef ARCH_HOST
	part = Storage::findDefaultPartition(Storage::Partition::SubType::Data::fat);
#endif

	DEFINE_FSTR_LOCAL(newfile_txt, "The name of this file is, perhaps, a little long.txt");

	auto fs = IFS::createFatFilesystem(part);

	int err = fs->mount();
	if(err == FS_OK) {
		fileSetFileSystem(fs);
	} else if(err == IFS::Error::BadFileSystem) {
		debug_i("Formatting disk");
		err = fs->format();

		debug_i("format: %s", fs->getErrorString(err).c_str());
		if(!fileMountFileSystem(fs)) {
			debug_e("Mount failed");
			return;
		}

		auto part = Storage::findDefaultPartition(Storage::Partition::SubType::Data::fwfs);
		fscopy(part, *getFileSystem());

		int err = fileSetContent(newfile_txt, F("It works!\r\n"));
		debug_i("fileSetContent(): %s", fileGetErrorString(err).c_str());
	} else {
		debug_e("Unhandled error during mount: %s", fs->getErrorString(err).c_str());
		delete fs;
		return;
	}

	String s = fileGetContent(newfile_txt);

	m_puts(_F("Read: "));
	m_nputs(s.c_str(), s.length());
	m_puts("\r\n");

	IFS::Debug::printFsInfo(Serial, *fs);
	String path;

#if 0
	IFS::Debug::listDirectory(Serial, *fs, path, IFS::Debug::Option::recurse);
	debug_i("*** Listing complete ***");

#ifdef ARCH_HOST
	fileSetFileSystem(nullptr);
	System.restart();
#endif

#else
	// listDirectoryAsync(fs, path);
#endif
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
