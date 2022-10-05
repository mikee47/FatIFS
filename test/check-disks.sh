# Script to verify generated disk image

set -ex

loopdev=$(sudo losetup -f)

check(){
    # fsck.fat can produce more verbose output than fsck.exfat
    part="$loopdev$1"
    fsck_cmd=$(sudo fsck -N $part | grep ' fsck\.(vfat|exfat)' -oE)
    args=""
    if [ $fsck_cmd = 'fsck.vfat' ]; then
        args=-lnvV
    elif [ $fsck_cmd = 'fsck.exfat' ]; then
        args=-n
    else
        echo "Error: No recognised filesystem"
        return 1
    fi
    # sudo $fsck_cmd $args $1 | tee -a $2
    sudo $fsck_cmd $args $part
    return $?
}

# First disk contains no partition table ('SFR')
sudo losetup -P $loopdev out/test1.img
sudo blkid -ip $loopdev
check ""
sudo losetup -d $loopdev

# Second disk has 4 partitions
sudo losetup -P $loopdev out/test2.img
sudo blkid -ip $loopdev*
check p1
check p2
check p3
check p4
sudo losetup -d $loopdev
