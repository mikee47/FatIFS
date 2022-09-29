#include "include/Storage/DiskPart.h"

String toString(Storage::DiskPart::SysType type)
{
	using Type = Storage::DiskPart::SysType;
	switch(type) {
	case Type::unknown:
		return F("unknown");
	case Type::fat12:
		return F("fat12");
	case Type::fat16:
		return F("fat16");
	case Type::fat32:
		return F("fat32");
	case Type::exfat:
		return F("exfat");
	}

	return nullptr;
}

namespace Storage
{
template <typename T, typename... Args> size_t tprintln(Print& p, String tag, const T& value, Args... args)
{
	size_t n{0};
	n += p.print(tag.padRight(16));
	n += p.print(": ");
	n += p.println(value, args...);
	return n;
}

size_t DiskPart::printTo(Print& p) const
{
	size_t n = Partition::printTo(p);

	if(mPart == nullptr || mPart->infosize != sizeof(Info)) {
		return n;
	}

	n += p.println();

	auto& info = *static_cast<const Info*>(mPart);

#define TPRINTLN(tag, value, ...) n += tprintln(p, F(tag), value, ##__VA_ARGS__)

	TPRINTLN("Sys Type", info.systype);
	if(info.guid) {
		TPRINTLN("GUID", info.guid);
	}
	if(info.sysind) {
		TPRINTLN("Sys Indicator", String(info.sysind, HEX, 2));
	}
	if(info.sectorSize || info.clusterSize) {
		TPRINTLN("Sector Size", info.sectorSize);
		TPRINTLN("Cluster Size", info.clusterSize);
	}

	return n;
}

} // namespace Storage
