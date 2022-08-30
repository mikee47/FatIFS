#include <SmingCore.h>

#include <IFS/FatFS.h>
#include <fatfs/ff.h>

namespace
{
#define FF_CHECK(func, err) debug_i(func "(): %s", fileGetErrorString(err).c_str())

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
