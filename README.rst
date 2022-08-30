FatIFS
======

IFS library for Sming supporting FAT filesystems.

Notes
-----

Each :cpp:class:`FatIFS::FileSystem` object handles a single FAT partition.
Maintain mapping of partitions to logical drive letters so paths can be
modified accordingly.

For example, "/dir1/root.txt" will be passed to fatfs as "A:\dir1\root.txt".
