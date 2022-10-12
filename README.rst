FatIFS
======

.. highlight:: c++

IFS library for Sming supporting FAT filesystems.

Formatting
----------

Use :cpp:func:`IFS::FAT::formatVolume` to format a partition::

   part = ... // Storage::Partition
   int err = IFS::FAT::formatVolume(part);
   Serial << "formatVolume: " << IFS::Error::toString(err) << endl;

An optional :cpp:struct:`IFS::FAT::FormatOptions` parameter can be used to provide additional settings::

   IFS::FAT::FormatOptions opt{
      .volumeLabel = "My FAT volume", // Volume label (distinct from partition name)
      .types = Storage::Disk::SysType::fat32, // Only use FAT32 for this volume
   };
   int err = IFS::FAT::formatVolume(part, opt);

For more detailed low-control over the formatting::

   IFS::FAT::FatParam param;
   int err = IFS::FAT::calculateFatParam(part, opt, param);
   if(err == FS_OK) {
      // .. adjust settings in `param` before formatting
      int err = IFS::FAT::formatVolume(part, param);
   }


Mounting FAT volumes
--------------------

Unlike SPIFFS or LittleFS, failure to mount a filing system does not auto-format the volume.
Instead, this is left to the application to handle. For example::

   auto fs = IFS::createFatFilesystem(part);
   if(fs != nullptr) {
      int err = fs->mount();
      if(err == Error::BadFileSystem) {
         // Filesystem layout is invalid...
         fs->format();
         err = fs->mount();
      }
      if(err != FS_OK) {
         // Handle error
      }
   }



Configuration options
---------------------

.. envvar:: ENABLE_EXFAT

   default: 0 (disabled)

   Set to 1 to enable support for EXFAT volumes.
   Requires :envvar:`ENABLE_STORAGE_SIZE64` option.


.. envvar:: ENABLE_FAT_TRIM

   default: disabled

   When using devices with TRIM support this setting should be enabled.
	See https://en.wikipedia.org/wiki/Trim_(computing).


.. envvar:: FAT_CODEPAGE

   default: 437 (US English)


Acknowledgements
----------------

- ChanN's fatfs library <http://elm-chan.org/fsw/ff/00index_e.html>`
- Linux Kernel <https://github.com/torvalds/linux/>
- Linux utilities https://github.com/util-linux/util-linux
- Linux FUSE exFAT filing system implementation https://github.com/relan/exfat


API Documentation
-----------------

.. doxygennamespace:: IFS::FAT
   :members:
