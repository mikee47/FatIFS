#include "diskdefs.h"
#include <debug_progmem.h>

namespace Storage
{
namespace diskdefs
{
uint32_t crc32_byte(uint32_t crc, uint8_t d)
{
	crc ^= d;
	for(unsigned i = 0; i < 8; ++i) {
		uint32_t mask = -(crc & 1);
		crc = (crc >> 1) ^ (0xEDB88320 & mask);
	}
	return crc;
}

uint32_t crc32(uint32_t bcc, const void* data, size_t length)
{
	bcc = ~bcc;
	auto ptr = static_cast<const uint8_t*>(data);
	while(length-- != 0) {
		bcc = crc32_byte(bcc, *ptr++);
	}
	return ~bcc;
}

// exFAT checksumming
uint32_t xsum32(uint8_t dat, uint32_t sum)
{
	return ((sum << 31) | (sum >> 1)) + dat;
}

uint32_t xsum32(const void* buffer, size_t length, uint32_t sum)
{
	auto pb = static_cast<const uint8_t*>(buffer);
	while(length-- != 0) {
		sum = xsum32(*pb++, sum);
	}
	return sum;
}

bool verifyGptHeader(gpt_header& gpt)
{
	/* Check signature, version (1.0) and length (92) */
	if(gpt.signature != GPT_HEADER_SIGNATURE) {
		return false;
	}
	if(gpt.revision != GPT_HEADER_REVISION_V1) {
		return false;
	}
	if(gpt.header_size != sizeof(gpt_header)) {
		return false;
	}

	uint32_t crc32_saved = gpt.header_crc32;
	gpt.header_crc32 = 0;
	uint32_t bcc = crc32(&gpt, sizeof(gpt));
	gpt.header_crc32 = crc32_saved;
	if(bcc != crc32_saved) {
		debug_e("[GPT] bcc 0x%08x, ~bcc 0x%08x, crc32 0x%08x", bcc, ~bcc, crc32_saved);
		return false;
	}
	if(gpt.sizeof_partition_entry != sizeof(gpt_entry)) {
		return false;
	}
	if(gpt.num_partition_entries > 128) {
		return false;
	}

	return true;
}

} // namespace diskdefs
} // namespace Storage
