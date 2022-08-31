fatfs modifications
===================

Setting mtime
-------------

Date/time is set at offset ``DIR_ModTime`` in the directory record.
This is set in ``f_sync()``.

Add ``modtime``, field to ``FFOBJID``.
This is set when opened, and updated when modified.
Change ``f_sync()`` to use this value when writing directory record.
