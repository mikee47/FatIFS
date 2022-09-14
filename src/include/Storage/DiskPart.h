#pragma once

#include <Storage/Device.h>
#include <Data/Uuid.h>

namespace Storage
{
struct DiskPart {
	enum class Type {
		unknown,
		invalid,
		fat,
		fat32,
		exfat,
	};

	Type type;
	uint64_t address;
	uint64_t size;
	String name;
	Uuid guid;
};

} // namespace Storage

String toString(Storage::DiskPart::Type type);
