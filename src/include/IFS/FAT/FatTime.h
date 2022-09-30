/****
 * FatTime.h
 *
 * This file is part of the FatIFS Library
 *
 * This library is free software: you can redistribute it and/or modify it under the terms of the
 * GNU General Public License as published by the Free Software Foundation, version 3 or later.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with this library.
 * If not, see <https://www.gnu.org/licenses/>.
 *
 */

#pragma once

#include <IFS/TimeStamp.h>
#include <DateTime.h>

/**
 * @brief FAT timestamp support
 */
union FatTime {
	static constexpr unsigned BaseYear{1980};

	struct {
		uint16_t time;
		uint16_t date;
	};
	struct {
		uint32_t second : 5;
		uint32_t minute : 6;
		uint32_t hour : 5;
		uint32_t day : 5;
		uint32_t month : 4;
		uint32_t year : 7;
	};
	uint32_t value;

	FatTime(uint32_t fdatetime = 0) : value(fdatetime)
	{
	}

	FatTime(uint16_t fdate, uint16_t ftime) : time(ftime), date(fdate)
	{
	}

	FatTime(IFS::TimeStamp ts) : FatTime(DateTime(ts))
	{
	}

	FatTime(DateTime dt)
		: second(dt.Second / 2U), minute(dt.Minute), hour(dt.Hour), day(dt.Day), month(dt.Month + 1U),
		  year(dt.Year - BaseYear)
	{
	}

	operator DateTime() const
	{
		DateTime dt;
		if(value != 0) {
			dt.setTime(second * 2, minute, hour, day, month - 1, year + BaseYear);
		}
		return dt;
	}

	explicit operator time_t() const
	{
		return DateTime(*this);
	}
};
static_assert(sizeof(FatTime) == 4, "Bad FatTime");
