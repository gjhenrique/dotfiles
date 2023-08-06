

# Setup machine
``` shell
pacman -S libvirt qemu-base qemu-img
systemctl enable libvirt
systemctl start libvirt
pacman -S cdrtools 
pacman -S qemuimg

virsh pool-define /dev/stdin <<EOF
<pool type='dir'>
  <name>default</name>
  <target>
    <path>/var/lib/libvirt/images</path>
  </target>
</pool>
EOF

virsh pool-start default
virsh pool-autostart default

virsh net-start default

mkdir -p /opt/work
```

# Stuck terraform

``` shell
virsh undefine work
```
