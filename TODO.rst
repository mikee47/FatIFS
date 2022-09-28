FatIFS TODO
===========

Partition code must operate on partition object, not device
    Pre-calculate all required fields to determine exact FAT variant, cluster size, etc.
    so partition table entry is correct.
    Formatting uses same information.

Decide how important os_type / sys_ind field is and how it gets set
    Ideally only write partition entry once. Only an issue for MBR.

DiskPart perhaps extraneous
    Only adds a few fields so these can be added to main Partition struct.
    This implies that `Partition` knows about these additional fields.

		Uuid guid;
		uint32_t clusterSize{}; ///< Cluster size (bytes)
		uint16_t sectorSize{};  ///< Sector size (bytes)
		SysType systype{};
		SysIndicator sysind{}; ///< Partition sys value

Sector size
    Need to be very clear about difference between Device sector size (DSS),
    and filing system sector size (FSS).

    Require FSS to be log2N multiple of DSS (e.g. x1, x2, x4, etc.)

    Byte-addressable devices could give a DSS of 0 or 1, perhaps.

    DSS of 4 indicates all reads and writes must be 32-bit word-aligned.
    exFAT specifies a value between 512 and 4096.

    Acceptable DSS values would therefore be 1, 512, 1024, 2048, 4096.

    Minimum FSS is 512. Maximum can be user-defined.

trim
    Best to keep this distinct from `erase_range` as it performs a subtly different function.
    Parameters are in sectors (DSS) but should probably change this to byte offsets for clarity.

Clarify terms
    Block
        Minimum region which can be erased either directly (via `erase_range`)
        or internally by SD card, etc.
    Sector
        Minimum read/write access unit for device.
        Always power of 2.
        Acceptable include 1, 512, 1024, 2048, 4096.
    LBA
        Filing system sector size.
        Must be log2N multiple of DSS (e.g. x1, x2, x4, etc.)
