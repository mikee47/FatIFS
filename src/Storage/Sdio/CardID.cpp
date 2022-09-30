#include <Storage/Sdio/CardID.h>

namespace Storage
{
namespace SDIO
{
size_t CardID::printTo(Print& p) const
{
	size_t n{0};

#define FIELD(tag, ...) n += p.print(_F("  " #tag ": " __VA_ARGS__));

	FIELD(MID, "0x")
	n += p.println(mid, HEX);

	FIELD(OID)
	n += p.write(oid, 2);
	n += p.println();

	FIELD(PNM)
	n += p.write(pnm, 5);
	n += p.println();

	FIELD(PRV)
	n += p.print(major());
	n += p.print('.');
	n += p.println(minor());

	FIELD(PSN)
	n += p.println(psn, HEX, 8);

	FIELD(MDT)
	n += p.print(mdt_month());
	n += p.print('/');
	n += p.println(mdt_year());

	return n;
}

} // namespace SDIO
} // namespace Storage
