#pragma once

#include <cstdint>
#include <memory>

namespace Storage
{
namespace diskdefs
{
class WorkBuffer : public std::unique_ptr<uint8_t[]>
{
public:
	WorkBuffer()
	{
	}

	WorkBuffer(size_t sectorSize, size_t sectorCount) : mSectorCount(sectorCount), mSize(sectorSize * sectorCount)
	{
		reset(new uint8_t[mSize]);
	}

	WorkBuffer& operator=(WorkBuffer&& other)
	{
		std::swap(mSectorCount, other.mSectorCount);
		std::swap(mSize, other.mSize);
		unique_ptr::swap(other);
		return *this;
	}

	template <typename T> T& as()
	{
		return *reinterpret_cast<T*>(get());
	}

	template <typename T> const T& as() const
	{
		return *reinterpret_cast<const T*>(get());
	}

	size_t size() const
	{
		return mSize;
	}

	uint32_t sectors() const
	{
		return mSectorCount;
	}

	void clear()
	{
		std::fill_n(get(), mSize, 0);
	}

private:
	size_t mSectorCount{0};
	size_t mSize{0};
};

} // namespace diskdefs
} // namespace Storage
