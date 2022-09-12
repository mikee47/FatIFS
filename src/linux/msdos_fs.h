/* SPDX-License-Identifier: GPL-2.0 WITH Linux-syscall-note */
#ifndef _UAPI_LINUX_MSDOS_FS_H
#define _UAPI_LINUX_MSDOS_FS_H

/*
 * The MS-DOS filesystem constants/structures
 */

#ifndef SECTOR_SIZE
#define SECTOR_SIZE 512 /* sector size (bytes) */
#endif
#define SECTOR_BITS 9		  /* log2(SECTOR_SIZE) */
#define MSDOS_DPB (MSDOS_DPS) /* dir entries per block */
#define MSDOS_DPB_BITS 4	  /* log2(MSDOS_DPB) */
#define MSDOS_DPS (SECTOR_SIZE / sizeof(struct msdos_dir_entry))
#define MSDOS_DPS_BITS 4   /* log2(MSDOS_DPS) */
#define MSDOS_LONGNAME 256 /* maximum name length */

#define MSDOS_ROOT_INO 1   /* The root inode number */
#define MSDOS_FSINFO_INO 2 /* Used for managing the FSINFO block */

#define MSDOS_DIR_BITS 5 /* log2(sizeof(struct msdos_dir_entry)) */

/* directory limit */
#define FAT_MAX_DIR_ENTRIES (65536)
#define FAT_MAX_DIR_SIZE (FAT_MAX_DIR_ENTRIES << MSDOS_DIR_BITS)

#define ATTR_NONE 0x0000   /* no attribute bits */
#define ATTR_RO 0x0001	 /* read-only */
#define ATTR_HIDDEN 0x0002 /* hidden */
#define ATTR_SYS 0x0004	/* system */
#define ATTR_VOLUME 0x0008 /* volume label */
#define ATTR_DIR 0x0010	/* directory */
#define ATTR_ARCH 0x0020   /* archived */

/* attribute bits that are copied "as is" */
#define ATTR_UNUSED (ATTR_VOLUME | ATTR_ARCH | ATTR_SYS | ATTR_HIDDEN)
/* bits that are used by the Windows 95/Windows NT extended FAT */
#define ATTR_EXT (ATTR_RO | ATTR_HIDDEN | ATTR_SYS | ATTR_VOLUME)

#define CASE_LOWER_BASE 8 /* base is lower case */
#define CASE_LOWER_EXT 16 /* extension is lower case */

#define DELETED_FLAG 0xe5 /* marks file as deleted when in name[0] */
#define IS_FREE(n) (!*(n) || *(n) == DELETED_FLAG)

#define FAT_LFN_LEN 255			   /* maximum long name length */
#define MSDOS_NAME 11			   /* maximum name length */
#define MSDOS_SLOTS 21			   /* max # of slots for short and long names */
#define MSDOS_DOT ".          "	/* ".", padded to MSDOS_NAME chars */
#define MSDOS_DOTDOT "..         " /* "..", padded to MSDOS_NAME chars */

/* start of data cluster's entry (number of reserved clusters) */
#define FAT_START_ENT 2

/* maximum number of clusters */
// #define MAX_FAT12 0xFF4
// #define MAX_FAT16 0xFFF4
// #define MAX_FAT32 0x0FFFFFF6

/* bad cluster mark */
#define BAD_FAT12 0xFF7
#define BAD_FAT16 0xFFF7
#define BAD_FAT32 0x0FFFFFF7

/* standard EOF */
#define EOF_FAT12 0xFFF
#define EOF_FAT16 0xFFFF
#define EOF_FAT32 0x0FFFFFFF

#define FAT_ENT_FREE (0)
#define FAT_ENT_BAD (BAD_FAT32)
#define FAT_ENT_EOF (EOF_FAT32)

#define FAT_FSINFO_SIG1 0x41615252
#define FAT_FSINFO_SIG2 0x61417272
#define IS_FSINFO(x) ((x)->signature1 == FAT_FSINFO_SIG1 && (x)->signature2 == FAT_FSINFO_SIG2)

#define FAT_STATE_DIRTY 0x01

struct __attribute__((packed)) fat_boot_sector_t {
	uint8_t jmp_boot[3];  /* Boot strap short or near jump */
	char system_id[8];	/* Name - can be used to special case partition manager volumes */
	uint16_t sector_size; /* bytes per logical sector */
	uint8_t sec_per_clus; /* sectors/cluster */
	uint16_t reserved;	/* reserved sectors */
	uint8_t num_fats;	 /* number of FATs */
	uint16_t dir_entries; /* root directory entries */
	uint16_t sectors;	 /* number of sectors */
	uint8_t media;		  /* media code */
	uint16_t fat_length;  /* sectors/FAT */
	uint16_t secs_track;  /* sectors per track */
	uint16_t heads;		  /* number of heads */
	uint32_t hidden;	  /* hidden sectors (unused) */
	uint32_t total_sect;  /* number of sectors (if sectors == 0) */

	union {
		struct __attribute__((packed)) {
			/*  Extended BPB Fields for FAT16 */
			uint8_t drive_number;		/* Physical drive number */
			uint8_t state;				/* undocumented, but used for mount state. */
			uint8_t signature;			/* extended boot signature */
			uint32_t vol_id;			/* volume ID */
			char vol_label[MSDOS_NAME]; /* volume label */
			uint64_t fs_type;			/* file system type */
										/* other fields are not added here */
		} fat16;

		struct __attribute__((packed)) {
			/* only used by FAT32 */
			uint32_t fat_length;   /* sectors/FAT */
			uint16_t flags;		   /* bit 8: fat mirroring, low 4: active fat */
			uint16_t version;	  /* major, minor filesystem version */
			uint32_t root_cluster; /* first cluster in root directory */
			uint16_t info_sector;  /* filesystem info sector */
			uint16_t backup_boot;  /* backup boot sector */
			uint16_t reserved2[6]; /* Unused */
			/* Extended BPB Fields for FAT32 */
			uint8_t drive_number;		/* Physical drive number */
			uint8_t state;				/* undocumented, but used for mount state. */
			uint8_t signature;			/* extended boot signature */
			uint32_t vol_id;			/* volume ID */
			char vol_label[MSDOS_NAME]; /* volume label */
			uint64_t fs_type;			/* file system type */
										/* other fields are not added here */
		} fat32;

		struct __attribute__((packed)) {
			uint8_t extra_info[510 - 36];
			uint16_t signature;
		};
	};
};

static_assert(offsetof(fat_boot_sector_t, signature) == 510, "Bad fat_boot_sector_t");

struct fat_boot_fsinfo_t {
	uint32_t signature1;	 /* 0x41615252L */
	uint32_t reserved1[120]; /* Nothing as far as I can tell */
	uint32_t signature2;	 /* 0x61417272L */
	uint32_t free_clusters;  /* Free cluster count.  -1 if unknown */
	uint32_t next_cluster;   /* Most recently allocated cluster */
	uint16_t reserved2[7];
	uint16_t signature;
};

static_assert(offsetof(fat_boot_fsinfo_t, signature) == 510, "Bad fat_boot_fsinfo_t");

struct msdos_dir_entry_t {
	uint8_t name[MSDOS_NAME];   /* name and extension */
	uint8_t attr;				/* attribute bits */
	uint8_t lcase;				/* Case for base and extension */
	uint8_t ctime_cs;			/* Creation time, centiseconds (0-199) */
	uint16_t ctime;				/* Creation time */
	uint16_t cdate;				/* Creation date */
	uint16_t adate;				/* Last access date */
	uint16_t starthi;			/* High 16 bits of cluster in FAT32 */
	uint16_t time, date, start; /* time, date and first cluster */
	uint32_t size;				/* file size (in bytes) */
};

static_assert(sizeof(msdos_dir_entry_t) == 32, "Bad msdos_dir_entry_t");

/* Up to 13 characters of the name */
struct msdos_dir_slot_t {
	uint8_t id;				/* sequence number for slot */
	uint8_t name0_4[10];	/* first 5 characters in name */
	uint8_t attr;			/* attribute byte */
	uint8_t reserved;		/* always 0 */
	uint8_t alias_checksum; /* checksum for 8.3 alias */
	uint8_t name5_10[12];   /* 6 more characters in name */
	uint16_t start;			/* starting cluster number, 0 in long slots */
	uint8_t name11_12[4];   /* last 2 characters in name */
};

#endif /* _UAPI_LINUX_MSDOS_FS_H */
