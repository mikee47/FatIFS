/*
Author: (github.com/)ADiea
Project: Sming for ESP8266 - https://github.com/anakod/Sming
License: MIT
Date: 15.07.2015
Descr: Low-level SDCard functions
*/
#pragma once

#include <Storage/Device.h>
#include <SPIBase.h>

namespace Storage
{
namespace SDIO
{
class Card : public Device
{
public:
	Card(SPIBase& spi) : spi(spi)
	{
	}

	bool begin(uint8_t chipSelect, uint32_t freqLimit = 0);

	bool read(uint64_t address, void* dst, size_t size) override;

	bool write(uint64_t address, const void* src, size_t size) override;

	bool erase_range(uint64_t address, size_t size) override
	{
		return false;
	}

	bool sync();

	String getName() const override
	{
		return F("SDCard");
	}

	Type getType() const
	{
		return Type::sdcard;
	}

	size_t getBlockSize() const override
	{
		return 1 << sectorSizeBits;
	}

	uint64_t getSize() const override
	{
		return uint64_t(sectorCount) << sectorSizeBits;
	}

	/**
	 * @brief Get erase block size in sectors
	 */
	uint32_t getEraseBlockSize() const
	{
		return 128;
	}

private:
	static constexpr uint8_t sectorSizeBits{9}; // 512 bytes per sector

	bool wait_ready();
	void deselect();
	bool select();
	bool rcvr_datablock(void* buff, size_t btr);
	bool xmit_datablock(const void* buff, uint8_t token);
	uint8_t send_cmd(uint8_t cmd, uint32_t arg);

	SPIBase& spi;
	uint32_t sectorCount{0};
	uint8_t chipSelect{255};
	bool initialised{false};
	uint8_t cardType; ///< b0:MMC, b1:SDv1, b2:SDv2, b3:Block addressing
};

} // namespace SDIO
} // namespace Storage
