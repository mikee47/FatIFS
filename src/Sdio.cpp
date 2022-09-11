/*
Modified by: (github.com/)ADiea
Project: Sming for ESP8266 - https://github.com/anakod/Sming
License: MIT
Date: 15.07.2015
Descr: Low-level SDCard functions
*/
/*------------------------------------------------------------------------/
/  Foolproof MMCv3/SDv1/SDv2 (in SPI mode) control module
/-------------------------------------------------------------------------/
/
/  Copyright (C) 2013, ChaN, all right reserved.
/
/ * This software is a free software and there is NO WARRANTY.
/ * No restriction on use. You can use, modify and redistribute it for
/   personal, non-profit or commercial products UNDER YOUR RESPONSIBILITY.
/ * Redistributions of source code must retain the above copyright notice.
/
/-------------------------------------------------------------------------/
  Features and Limitations:

  * Easy to Port Bit-banging SPI
    It uses only four GPIO pins. No complex peripheral needs to be used.

  * Platform Independent
    You need to modify only a few macros to control the GPIO port.

  * Low Speed
    The data transfer rate will be several times slower than hardware SPI.

  * No Media Change Detection
    Application program needs to perform a f_mount() after media change.

/-------------------------------------------------------------------------*/

#include "include/Storage/Sdio.h"
#include "include/Storage/DiskScanner.h"
#include <Clock.h>
#include <debug_progmem.h>

#define SECTOR_SIZE 512

/* MMC/SD command (SPI mode) */
#define CMD0 (0)		   /* GO_IDLE_STATE */
#define CMD1 (1)		   /* SEND_OP_COND */
#define ACMD41 (0x80 + 41) /* SEND_OP_COND (SDC) */
#define CMD8 (8)		   /* SEND_IF_COND */
#define CMD9 (9)		   /* SEND_CSD */
#define CMD10 (10)		   /* SEND_CID */
#define CMD12 (12)		   /* STOP_TRANSMISSION */
#define CMD13 (13)		   /* SEND_STATUS */
#define ACMD13 (0x80 + 13) /* SD_STATUS (SDC) */
#define CMD16 (16)		   /* SET_BLOCKLEN */
#define CMD17 (17)		   /* READ_SINGLE_BLOCK */
#define CMD18 (18)		   /* READ_MULTIPLE_BLOCK */
#define CMD23 (23)		   /* SET_BLOCK_COUNT */
#define ACMD23 (0x80 + 23) /* SET_WR_BLK_ERASE_COUNT (SDC) */
#define CMD24 (24)		   /* WRITE_BLOCK */
#define CMD25 (25)		   /* WRITE_MULTIPLE_BLOCK */
#define CMD32 (32)		   /* ERASE_ER_BLK_START */
#define CMD33 (33)		   /* ERASE_ER_BLK_END */
#define CMD38 (38)		   /* ERASE */
#define CMD55 (55)		   /* APP_CMD */
#define CMD58 (58)		   /* READ_OCR */

/* MMC card type flags (MMC_GET_TYPE) */
#define CT_MMC 0x01				 /* MMC ver 3 */
#define CT_SD1 0x02				 /* SD ver 1 */
#define CT_SD2 0x04				 /* SD ver 2 */
#define CT_SDC (CT_SD1 | CT_SD2) /* SD */
#define CT_BLOCK 0x08			 /* Block addressing */

namespace Storage
{
namespace SDIO
{
/*-----------------------------------------------------------------------*/
/* Wait for card ready                                                   */
/*-----------------------------------------------------------------------*/
bool Card::wait_ready() /* 1:OK, 0:Timeout */
{
	for(unsigned tmr = 5000; tmr; tmr--) { /* Wait for ready in timeout of 500ms */
		uint8_t d = spi.transfer(0xff);
		if(d == 0xFF) {
			return true;
		}
		delayMicroseconds(100);
	}

	return false;
}

/*-----------------------------------------------------------------------*/
/* Deselect the card and release SPI bus                                 */
/*-----------------------------------------------------------------------*/

void Card::deselect()
{
	digitalWrite(chipSelect, HIGH);
	spi.transfer(0xff); /* Send 0xFF Dummy clock (force DO hi-z for multiple slave SPI) */
}

/*-----------------------------------------------------------------------*/
/* Select the card and wait for ready                                    */
/*-----------------------------------------------------------------------*/

bool Card::select() /* 1:OK, 0:Timeout */
{
	digitalWrite(chipSelect, LOW);
	spi.transfer(0xff); /* Dummy clock (force DO enabled) */
	if(wait_ready()) {
		return true;
	}

	debug_e("SDCard select() failed");
	deselect();
	return false;
}

/*
 * Receive a data packet from the card
 */
bool Card::rcvr_datablock(void* buff, size_t btr)
{
	/* Wait for data packet in timeout of 100ms */
	uint8_t d{0xFF};
	for(unsigned tmr = 1000; tmr; tmr--) {
		d = spi.transfer(0xff);
		if(d != 0xFF) {
			break;
		}
		delayMicroseconds(100);
	}
	if(d != 0xFE) {
		return false; /* If not valid data token, return with error */
	}

	memset(buff, 0xFF, btr);
	spi.transfer(static_cast<uint8_t*>(buff), btr);
	spi.transfer16(0xffff); // keep MOSI HIGH, discard CRC

	// success
	return true;
}

/*
 * Send a data packet to the card
 */
bool Card::xmit_datablock(const void* buff, uint8_t token)
{
	if(!wait_ready()) {
		debug_e("[SDCard] wait_ready failed");
		return false;
	}

	spi.transfer(token); /* Xmit a token */
	if(token != 0xFD) {  /* Is it data token? */
		// Data gets modified so take a copy
		uint8_t buffer[SECTOR_SIZE];
		memcpy(buffer, buff, sizeof(buffer));
		spi.transfer(buffer, sizeof(buffer)); /* Xmit the 512 byte data block to MMC */

		//		spi.setMOSI(HIGH); /* Send 0xFF */
		spi.transfer16(0xffff);			/* Xmit dummy CRC */
		uint8_t d = spi.transfer(0xff); /* keep MOSI HIGH and receive data response */

		if((d & 0x1F) != 0x05) { /* If not accepted, return with error */
			debug_e("[SDCard] data not accepted, d = 0x%02x", d);
			return false;
		}
	}

	return true;
}

/*
 * Send a command packet to the card
 *
 * @retval uint8_t Command response (bit7: Send failed)
 */
uint8_t Card::send_cmd(uint8_t cmd, uint32_t arg)
{
	if(cmd & 0x80) { /* ACMD<n> is the command sequence of CMD55-CMD<n> */
		cmd &= 0x7F;
		uint8_t n = send_cmd(CMD55, 0);
		if(n > 1) {
			debug_e("[SDCard] CMD55 error, n = 0x%02x", n);
			return n;
		}
	}

	/* Select the card and wait for ready except to stop multiple block read */
	if(cmd != CMD12) {
		deselect();
		if(!select()) {
			debug_e("[SDCard] Select failed");
			return 0xFF;
		}
	}

	/* Send a command packet */
	uint8_t crc;
	if(cmd == CMD0) {
		crc = 0x95; /* (valid CRC for CMD0(0)) */
	} else if(cmd == CMD8) {
		crc = 0x87; /* (valid CRC for CMD8(0x1AA)) */
	} else {
		crc = 0x01; /* Dummy CRC + Stop */
	}
	uint8_t buf[]{
		uint8_t(0x40 | cmd), /* Start + Command index */
		uint8_t(arg >> 24),  /* Argument[31..24] */
		uint8_t(arg >> 16),  /* Argument[23..16] */
		uint8_t(arg >> 8),   /* Argument[15..8] */
		uint8_t(arg),		 /* Argument[7..0] */
		crc,
		0xff, /* Dummy clock (force DO enabled) */
	};
	spi.transfer(buf, sizeof(buf));

	/* Receive command response */
	if(cmd == CMD12) {
		spi.transfer(0xff); /* Skip a stuff byte when stop reading */
	}

	/* Wait for a valid response */
	uint8_t d;
	unsigned n = 10;
	do {
		d = spi.transfer(0xff);
	} while((d & 0x80) && --n);

	//	debug_i("SDcard send_cmd %u -> %u (%u try)", cmd, d, n);
	return d; /* Return with the response value */
}

bool Card::begin(uint8_t chipSelect, uint32_t freqLimit)
{
	if(initialised) {
		return false;
	}

	this->chipSelect = chipSelect;
	digitalWrite(chipSelect, HIGH);
	pinMode(chipSelect, OUTPUT);
	digitalWrite(chipSelect, HIGH);

	if(!spi.begin()) {
		debug_e("SDCard SPI init failed");
		return false;
	}

	const uint32_t maxFreq{40000000U};
	auto freq = (freqLimit > 0) && (freqLimit < maxFreq) ? freqLimit : maxFreq;
	SPISettings settings(freq, MSBFIRST, SPI_MODE0);
	spi.beginTransaction(settings);

	delayMicroseconds(10000);

	debug_i("disk_initialize: send 80 0xFF cycles");
	uint8_t tmp[80 / 8];
	memset(tmp, 0xff, sizeof(tmp));
	spi.transfer(tmp, sizeof(tmp));

	//	debug_i("disk_initialize: send n send_cmd(CMD0, 0)");
	uint8_t retCmd;
	uint8_t n = 5;
	do {
		retCmd = send_cmd(CMD0, 0);
		n--;
	} while(n && retCmd != 1);
	debug_i("disk_initialize: until n = 5 && ret != 1");

	uint8_t ty = 0;
	if(retCmd == 1) {
		debug_i("disk_initialize: Enter Idle state, send_cmd(CMD8, 0x1AA) == 1");
		/* Enter Idle state */
		if(send_cmd(CMD8, 0x1AA) == 1) { /* SDv2? */
			debug_i("[SDCard] Sdv2 ?");
			uint8_t buf[4]{0xff, 0xff, 0xff, 0xff};
			spi.transfer(buf, sizeof(buf));
			debug_hex(INFO, "[SDCard]", buf, sizeof(buf));
			if(buf[2] == 0x01 && buf[3] == 0xAA) { /* The card can work at vdd range of 2.7-3.6V */
				unsigned tmr;
				for(tmr = 1000; tmr; tmr--) { /* Wait for leaving idle state (ACMD41 with HCS bit) */
					if(send_cmd(ACMD41, 1UL << 30) == 0) {
						debug_i("[SDCard] ACMD41 OK");
						break;
					}
					delayMicroseconds(1000);
				}
				if(tmr == 0) {
					debug_i("[SDCard] ACMD41 FAIL");
				}
				if(tmr != 0 && send_cmd(CMD58, 0) == 0) { /* Check CCS bit in the OCR */
														  //					spi.setMOSI(HIGH); /* Send 0xFF */
														  //					spi.recv(buf, 4);
					//					ty = (buf[0] & 0x40) ? CT_SD2 | CT_BLOCK : CT_SD2;	/* SDv2 */
					memset(buf, 0xFF, sizeof(buf));
					spi.transfer(buf, sizeof(buf));
					ty = (buf[0] & 0x40) ? CT_SD2 | CT_BLOCK : CT_SD2; /* SDv2 */
					debug_hex(INFO, "[SDCard]", buf, sizeof(buf));
				}
			}
		} else { /* SDv1 or MMCv3 */
			debug_i("[SDCard] Sdv1 / MMCv3 ?");
			uint8_t cmd;
			if(send_cmd(ACMD41, 0) <= 1) {
				ty = CT_SD1;
				cmd = ACMD41; /* SDv1 */
			} else {
				ty = CT_MMC;
				cmd = CMD1; /* MMCv3 */
			}
			unsigned tmr;
			for(tmr = 1000; tmr; tmr--) { /* Wait for leaving idle state */
				if(send_cmd(cmd, 0) == 0) {
					break;
				}
				delayMicroseconds(1000);
			}
			/* Set R/W block length to 512 */
			if(tmr == 0 || send_cmd(CMD16, getBlockSize()) != 0) {
				debug_i("[SDCard] tmr = 0 || CMD16 != 0");
				ty = 0;
			}
		}
	} else {
		debug_e("SDCard ERROR: %x", retCmd);
	}

	if(ty != 0) {
		// Get number of sectors on the disk (uint32_t)
		uint8_t csd[16];
		if(send_cmd(CMD9, 0) == 0 && rcvr_datablock(csd, 16)) {
			if((csd[0] >> 6) == 1) { /* SDC ver 2.00 */
				uint32_t cs = csd[9] + (uint16_t(csd[8]) << 8) + (uint32_t(csd[7] & 63) << 16) + 1;
				sectorCount = cs << 10;
			} else { /* SDC ver 1.XX or MMC */
				uint8_t n = (csd[5] & 15) + ((csd[10] & 128) >> 7) + ((csd[9] & 3) << 1) + 2;
				uint32_t cs = (csd[8] >> 6) + (uint16_t(csd[7]) << 2) + (uint16_t(csd[6] & 3) << 10) + 1;
				sectorCount = cs << (n - 9);
			}
		} else {
			ty = 0;
		}
	}

	cardType = ty;

	if(ty == 0) {
		debug_e("SDCard init FAIL");
	} else {
		initialised = true;
		debug_i("SDCard OK: TYPE %u", ty);
	}

	deselect();

	if(initialised) {
		scanDiskPartitions(*this);
	}

	return initialised;
}

bool Card::read(uint64_t address, void* dst, size_t size)
{
	if(!initialised) {
		return false;
	}

	if(address % SECTOR_SIZE != 0 || size % SECTOR_SIZE != 0 || size == 0) {
		debug_e("[SDIO] Read must be whole sectors");
		return false;
	}

	// Convert byte address to sector number for block devices
	if(cardType & CT_BLOCK) {
		address /= SECTOR_SIZE;
	}

	auto blockCount = size / SECTOR_SIZE;
	uint8_t cmd = (blockCount > 1) ? CMD18 : CMD17; /*  READ_MULTIPLE_BLOCK : READ_SINGLE_BLOCK */
	if(send_cmd(cmd, address) == 0) {
		auto bufptr = static_cast<uint8_t*>(dst);
		do {
			if(!rcvr_datablock(bufptr, SECTOR_SIZE)) {
				debug_e("[SDCard] rcvr error");
				break;
			}
			bufptr += SECTOR_SIZE;
			--blockCount;
		} while(blockCount != 0);
		if(cmd == CMD18) {
			send_cmd(CMD12, 0); /* STOP_TRANSMISSION */
		}
	}
	deselect();

	return blockCount == 0;
}

bool Card::write(uint64_t address, const void* src, size_t size)
{
	debug_i("[SDIO] write (%llu, %u)", address, size);
	// m_printHex("READ", src, size);

	if(!initialised) {
		return false;
	}

	if(address % SECTOR_SIZE != 0 || size % SECTOR_SIZE != 0 || size == 0) {
		debug_e("[SDIO] Write must be whole sectors");
		return false;
	}

	// Convert byte address to sector number for block devices
	if(cardType & CT_BLOCK) {
		address /= SECTOR_SIZE;
	}

	auto blockCount = size / SECTOR_SIZE;
	if(blockCount == 1) { /* Single block write */
		if((send_cmd(CMD24, address) == 0) && xmit_datablock(src, 0xFE)) {
			blockCount = 0;
		} else {
			debug_e("[SDCard] CMD24 error");
		}
	} else { /* Multiple block write */
		if(cardType & CT_SDC) {
			send_cmd(ACMD23, blockCount);
		}
		if(send_cmd(CMD25, address) == 0) { /* WRITE_MULTIPLE_BLOCK */
			auto bufptr = static_cast<const uint8_t*>(src);
			do {
				if(!xmit_datablock(bufptr, 0xFC)) {
					debug_e("[SDCard] xmit error");
					break;
				}
				bufptr += SECTOR_SIZE;
				--blockCount;
			} while(blockCount != 0);

			if(!xmit_datablock(0, 0xFD)) { /* STOP_TRAN token */
				debug_e("[SDCard] STOP_TRAN error");
				blockCount = 1;
			}
		}
	}
	deselect();

	return blockCount == 0;
}

bool Card::sync()
{
	if(!initialised) {
		return false;
	}

	// Make sure that no pending write process
	bool res = select();
	deselect();
	return res;
}

} // namespace SDIO
} // namespace Storage
