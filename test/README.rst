FatIFS test
===========

.. highlight:: bash

Application to test Sming FAT filesystem integration.

Except on Windows systems, the ``execute`` build target runs the ``check-disks.sh`` script
to verify the generated filesystem images.
It requires Linux exFAT support, which should be available for Kernel 5.4 or higher.
Support for earlier versions can be added using FUSE::

    sudo apt-get install exfat-fuse exfat-utils
