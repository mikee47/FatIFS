/* SPDX-License-Identifier: GPL-2.0-or-later */
/*
 * Copyright (C) 2012-2013 Samsung Electronics Co., Ltd.
 */

#ifndef _EXFAT_RAW_H
#define _EXFAT_RAW_H

#define BOOT_SIGNATURE 0xAA55
#define EXBOOT_SIGNATURE 0xAA550000
#define STR_EXFAT "EXFAT   " /* size should be 8 */

#define EXFAT_MAX_FILE_LEN 255

#define VOLUME_DIRTY 0x0002
#define MEDIA_FAILURE 0x0004

#define EXFAT_EOF_CLUSTER 0xFFFFFFFFu
#define EXFAT_BAD_CLUSTER 0xFFFFFFF7u
#define EXFAT_FREE_CLUSTER 0
/* Cluster 0, 1 are reserved, the first cluster is 2 in the cluster heap. */
#define EXFAT_RESERVED_CLUSTERS 2
#define EXFAT_FIRST_CLUSTER 2
#define EXFAT_DATA_CLUSTER_COUNT(sbi) ((sbi)->num_clusters - EXFAT_RESERVED_CLUSTERS)

/* AllocationPossible and NoFatChain field in GeneralSecondaryFlags Field */
#define ALLOC_FAT_CHAIN 0x01
#define ALLOC_NO_FAT_CHAIN 0x03

#define DENTRY_SIZE 32 /* directory entry size */
#define DENTRY_SIZE_BITS 5
/* exFAT allows 8388608(256MB) directory entries */
#define MAX_EXFAT_DENTRIES 8388608

/* dentry types */
#define EXFAT_UNUSED 0x00 /* end of directory */
#define EXFAT_DELETE (~0x80)
#define IS_EXFAT_DELETED(x) ((x) < 0x80) /* deleted file (0x01~0x7F) */
#define EXFAT_INVAL 0x80				 /* invalid value */
#define EXFAT_BITMAP 0x81				 /* allocation bitmap */
#define EXFAT_UPCASE 0x82				 /* upcase table */
#define EXFAT_VOLUME 0x83				 /* volume label */
#define EXFAT_FILE 0x85					 /* file or dir */
#define EXFAT_GUID 0xA0
#define EXFAT_PADDING 0xA1
#define EXFAT_ACLTAB 0xA2
#define EXFAT_STREAM 0xC0 /* stream entry */
#define EXFAT_NAME 0xC1   /* file name entry */
#define EXFAT_ACL 0xC2	/* stream entry */

#define IS_EXFAT_CRITICAL_PRI(x) (x < 0xA0)
#define IS_EXFAT_BENIGN_PRI(x) (x < 0xC0)
#define IS_EXFAT_CRITICAL_SEC(x) (x < 0xE0)

/* checksum types */
#define CS_DIR_ENTRY 0
#define CS_BOOT_SECTOR 1
#define CS_DEFAULT 2

/* file attributes */
#define ATTR_READONLY 0x0001
#define ATTR_HIDDEN 0x0002
#define ATTR_SYSTEM 0x0004
#define ATTR_VOLUME 0x0008
#define ATTR_SUBDIR 0x0010
#define ATTR_ARCHIVE 0x0020

#define ATTR_RWMASK (ATTR_HIDDEN | ATTR_SYSTEM | ATTR_VOLUME | ATTR_SUBDIR | ATTR_ARCHIVE)

#define BOOTSEC_JUMP_BOOT_LEN 3
#define BOOTSEC_OLDBPB_LEN 53

#define EXFAT_FILE_NAME_LEN 15

#define EXFAT_MIN_SECT_SIZE_BITS 9
#define EXFAT_MAX_SECT_SIZE_BITS 12
#define EXFAT_MAX_SECT_PER_CLUS_BITS(x) (25 - (x)->sect_size_bits)

/* EXFAT: Main and Backup Boot Sector (512 bytes) */
struct __attribute__((packed)) boot_sector_t {
	uint8_t jmp_boot[BOOTSEC_JUMP_BOOT_LEN];
	uint64_t fs_type;
	uint8_t must_be_zero[BOOTSEC_OLDBPB_LEN];
	uint64_t partition_offset;
	uint64_t vol_length;
	uint32_t fat_offset;
	uint32_t fat_length;
	uint32_t clu_offset;
	uint32_t clu_count;
	uint32_t root_cluster;
	uint32_t vol_serial;
	uint16_t fs_revision;
	uint16_t vol_flags;
	uint8_t sect_size_bits;
	uint8_t sect_per_clus_bits;
	uint8_t num_fats;
	uint8_t drv_sel;
	uint8_t percent_in_use;
	uint8_t reserved[7];
	uint8_t boot_code[390];
	uint16_t signature;
};

struct __attribute__((packed)) exfat_dentry_t {
	uint8_t type;
	union {
		struct __attribute__((packed)) {
			uint8_t num_chars;
			char label[22];
			uint8_t reserved;
		} volume_label;
		/* file directory entry */
		struct __attribute__((packed)) {
			uint8_t num_ext;
			uint16_t checksum;
			uint16_t attr;
			uint16_t reserved1;
			uint16_t create_time;
			uint16_t create_date;
			uint16_t modify_time;
			uint16_t modify_date;
			uint16_t access_time;
			uint16_t access_date;
			uint8_t create_time_cs;
			uint8_t modify_time_cs;
			uint8_t create_tz;
			uint8_t modify_tz;
			uint8_t access_tz;
			uint8_t reserved2[7];
		} file;
		/* stream extension directory entry */
		struct __attribute__((packed)) {
			uint8_t flags;
			uint8_t reserved1;
			uint8_t name_len;
			uint16_t name_hash;
			uint16_t reserved2;
			uint64_t valid_size;
			uint32_t reserved3;
			uint32_t start_clu;
			uint64_t size;
		} stream;
		/* file name directory entry */
		struct __attribute__((packed)) {
			uint8_t flags;
			uint16_t unicode_0_14[EXFAT_FILE_NAME_LEN];
		} name;
		/* allocation bitmap directory entry */
		struct __attribute__((packed)) {
			uint8_t flags;
			uint8_t reserved[18];
			uint32_t start_clu;
			uint64_t size;
		} bitmap;
		/* up-case table directory entry */
		struct __attribute__((packed)) {
			uint8_t reserved1[3];
			uint32_t checksum;
			uint8_t reserved2[12];
			uint32_t start_clu;
			uint64_t size;
		} upcase;
	};
};

static_assert(sizeof(exfat_dentry_t) == 32, "Bad exfat_dentry_t");

#define EXFAT_TZ_VALID (1 << 7)

/* Jan 1 GMT 00:00:00 1980 */
#define EXFAT_MIN_TIMESTAMP_SECS 315532800LL
/* Dec 31 GMT 23:59:59 2107 */
#define EXFAT_MAX_TIMESTAMP_SECS 4354819199LL

#endif /* !_EXFAT_RAW_H */
