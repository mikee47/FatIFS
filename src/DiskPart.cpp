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
size_t DiskPart::printTo(Print& p) const
{
	size_t n{0};
	n += p.print(_F("Type "));
	n += p.println(type);
	n += p.print(_F("Address 0x"));
	n += p.println(address, HEX);
	n += p.print(_F("Size 0x"));
	n += p.println(size, HEX);
	n += p.print(_F("Name "));
	n += p.println(name);
	n += p.print(_F("GUID "));
	n += p.println(guid);
	n += p.print(_F("Num Fat "));
	n += p.println(numFat);
	n += p.print(_F("Sector Size 0x"));
	n += p.println(sectorSize);
	n += p.print(_F("Cluster Size 0x"));
	n += p.println(clusterSize);
	return n;
}

} // namespace Storage
