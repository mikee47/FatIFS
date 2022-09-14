#include "include/Storage/DiskPart.h"

String toString(Storage::DiskPart::Type type)
{
	using Type = Storage::DiskPart::Type;
	switch(type) {
	case Type::unknown:
		return F("unknown");
	case Type::invalid:
		return F("invalid");
	case Type::fat:
		return F("fat");
	case Type::fat32:
		return F("fat32");
	case Type::exfat:
		return F("exfat");
	}

	return nullptr;
}
