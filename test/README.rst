FatIFS test
===========

.. highlight:: bash

Application to test Sming FAT filesystem integration.

Except on Windows systems, the ``execute`` build target runs the ``check-disks.sh`` script
to verify the generated filesystem images.
It requires Linux exFAT support, which should be available for Kernel 5.4 or higher.
Support for earlier versions can be added using FUSE::

    sudo apt-get install exfat-fuse exfat-utils


.. note::

   SPIFFS is not a good match for block devices, mainly because it expects erased state to be 0xFF instead of 0.
   It also lacks directory support and long filename support.

   LittleFS appears to work without issue.
