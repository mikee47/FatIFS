#include "include/Storage/DiskPart.h"
#include <FlashString/Array.hpp>
#include "../linux/efi.h"

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
String DiskPart::Info::getTypeName() const
{
	struct Entry {
		const efi_guid_t* guid;
		const FlashString* name;
	};
#define XX(name, ...) DEFINE_FSTR_LOCAL(FS_##name, #name)
	EFI_PARTITION_TYPE_GUID_MAP(XX)
#undef XX
#define XX(name, ...) {&name##_GUID, &FS_##name},
	DEFINE_FSTR_ARRAY_LOCAL(list, Entry, EFI_PARTITION_TYPE_GUID_MAP(XX))
#undef XX

	for(auto e : list) {
		if(*e.guid == typeGuid) {
			return *e.name;
		}
	}

	return nullptr;
}

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
	if(info.typeGuid || info.uniqueGuid) {
		String typeName = info.getTypeName();
		if(typeName) {
			TPRINTLN("EFI Type", typeName);
		}
		TPRINTLN("EFI Type GUID", info.typeGuid);
		TPRINTLN("EFI Unique GUID", info.uniqueGuid);
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
