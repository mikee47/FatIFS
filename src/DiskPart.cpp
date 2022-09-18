#include "include/Storage/DiskPart.h"

String toString(Storage::DiskPart::Type type)
{
	using Type = Storage::DiskPart::Type;
	switch(type) {
	case Type::unknown:
		return F("unknown");
	case Type::invalid:
		return F("invalid");
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
	size_t n{0};

#define TPRINTLN(tag, value, ...) n += tprintln(p, F(tag), value, ##__VA_ARGS__)

	TPRINTLN("Type", type);
	TPRINTLN("Address", "0x" + String(address, HEX));
	TPRINTLN("Size", "0x" + String(size, HEX));
	TPRINTLN("Name", name);
	TPRINTLN("GUID", guid);
	TPRINTLN("Num Fat", numFat);
	TPRINTLN("Sector Size", sectorSize);
	TPRINTLN("Cluster Size", clusterSize);

	return n;
}

} // namespace Storage
