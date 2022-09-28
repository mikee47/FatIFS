sudo losetup -P /dev/loop0 gentest.img
sudo fsck /dev/loop0
sudo mount /dev/loop0 tmp
