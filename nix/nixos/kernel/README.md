### Origin

```
origin	git://kernel.ubuntu.com/ubuntu/ubuntu-utopic.git (fetch)
origin	git://kernel.ubuntu.com/ubuntu/ubuntu-utopic.git (push)

commit 904d58943bc81685931fcef005bb04dcd5b2430d
Author: Kamal Mostafa <kamal@canonical.com>
Date:   Mon Dec 15 12:35:27 2014 -0800

    UBUNTU: Ubuntu-3.16.0-29.39
    
    Signed-off-by: Kamal Mostafa <kamal@canonical.com>
```

### Post-processing

```bash
cp debian.master/control.d/generic.inclusion-list ./
sed 's/mv/cp/g' debian/scripts/module-inclusion > ./module-inclusion
for modtype in $(grep -v nfs ./debian.master/d-i/exclude-modules.amd64-virtual); do \
    cat ./debian.master/d-i/modules/$modtype; \
done | awk '{print $1}' | sed '/.ko$/!s/$/.ko/' > exclude.amd64-virtual
```
