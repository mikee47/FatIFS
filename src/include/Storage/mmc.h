#pragma once

#include <cstdint>

namespace Storage
{
namespace SDIO
{
/*
	Samsung 32GB EVO Plus CID: 1b 53 4d 45 42 31 51 54 30 f1 77 5f ea 01 1a b9  .SMEB1QT0.w_....

	1b                  MID
	53 4d	            OID: "SM"
	45 42 31 51 54 30   PNM: "EB1QT"
	30                  PRV
	f1 77 5f ea         PSN
	1a 01               MDT    0000 0001 0001 1010
	b9                  CRC7: 0x5C
*/
struct __attribute__((packed)) cid_t {
	uint8_t mid;		  ///< Manufacturer ID
	char oid[2];		  ///< OEM / Application ID
	char pnm[5];		  ///< Product name
	uint8_t prv;		  ///< Product revision
	uint32_t psn;		  ///< Product serial number
	uint16_t mdt;		  ///< Manufacturing date
	uint8_t not_used : 1; ///< Always 1
	uint8_t crc7 : 7;	 ///< 7-bit checksum

	void bswap()
	{
		psn = __builtin_bswap32(psn);
		mdt = __builtin_bswap16(mdt);
	}

	uint16_t mdt_year() const
	{
		return 2000 + ((mdt >> 4) & 0xff);
	}

	uint8_t mdt_month() const
	{
		return mdt & 0x0f;
	}

	uint8_t major() const
	{
		return prv >> 4;
	}

	uint8_t minor() const
	{
		return prv & 0x0f;
	}
};
static_assert(sizeof(cid_t) == 16, "Bad CID struct");

} // namespace SDIO
} // namespace Storage
