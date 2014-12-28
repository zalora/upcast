### Origin

```console
% git remote -v
origin	git://kernel.ubuntu.com/ubuntu/ubuntu-utopic.git (fetch)
origin	git://kernel.ubuntu.com/ubuntu/ubuntu-utopic.git (push)

% git log -1
commit 904d58943bc81685931fcef005bb04dcd5b2430d
Author: Kamal Mostafa <kamal@canonical.com>
Date:   Mon Dec 15 12:35:27 2014 -0800

    UBUNTU: Ubuntu-3.16.0-29.39
    
    Signed-off-by: Kamal Mostafa <kamal@canonical.com>
```

### Post-processing

```bash
cp debian.master/control.d/generic.inclusion-list ./

sed 's/mv/cp -r/g' debian/scripts/module-inclusion > ./module-inclusion

for modtype in $(grep -v nfs ./debian.master/d-i/exclude-modules.amd64-virtual); do \
    cat ./debian.master/d-i/modules/$modtype; \
done | awk '{print $1}' | sed '/.ko$/!s/$/.ko/' > exclude.amd64-virtual

cat >> ./generic.inclusion-list << EOF
fs/ext3/ext3.ko
fs/ext4/ext4.ko
fs/jbd/jbd.ko
fs/jbd2/jbd2.ko
fs/mbcache.ko
net/packet/*
net/unix/*
drivers/block/xen-blkfront.ko
drivers/video/console/bitblit.ko
drivers/video/console/fbcon.ko
drivers/video/console/softcursor.ko
drivers/video/console/tileblit.ko
drivers/video/fb_sys_fops.ko
drivers/video/syscopyarea.ko
drivers/video/sysfillrect.ko
drivers/video/sysimgblt.ko
drivers/video/xen-fbfront.ko
drivers/virtio/*
drivers/char/hw_random/rng-core.ko
drivers/char/tpm/xen-tpmfront.ko
drivers/watchdog/xen_wdt.ko
drivers/input/misc/xen-kbdfront.ko
drivers/video/xen-fbfront.ko
drivers/pci/xen-pcifront.ko
drivers/block/xen-blkfront.ko
drivers/block/xen-blkback/xen-blkback.ko
drivers/net/xen-netback/xen-netback.ko
drivers/net/ethernet/qlogic/netxen/netxen_nic.ko
drivers/net/xen-netfront.ko
drivers/xen/xenfs/xenfs.ko
drivers/xen/xen-evtchn.ko
drivers/xen/xen-pciback/xen-pciback.ko
drivers/xen/xen-acpi-processor.ko
drivers/xen/xen-gntalloc.ko
drivers/xen/xen-privcmd.ko
drivers/xen/xen-gntdev.ko
EOF
```
