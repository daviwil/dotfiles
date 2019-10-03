# Guix Configuration

## Building the Installation Image

Since I use modern Thinkpads, I have to use the non-free kernel and firmware
blobs from the [`nonguix`](https://gitlab.com/nonguix/nonguix) channel.  After
cloning the repo, the installation image can be built with this command:

```shell
guix system disk-image ~/Projects/Code/nonguix/nongnu/system/install.scm
```

**NOTE:** It can take an hour or more for this to complete, so be patient...

Once the build is complete, you can write the output image to a USB stick:

```shell
sudo dd if=/gnu/store/nyg6jv3a4l0pbcvb0x7jfsb60k9qalga-disk-image of=/dev/sdX status=progress
```

## Installing Guix

With the newly "burned" installation image, reboot into the graphical installer
and go through the steps.  I use the [graphical
installer](https://guix.gnu.org/manual/en/html_node/Guided-Graphical-Installation.html#Guided-Graphical-Installation)
(for now) because it's much easier to set up encrypted volumes that way.  Once
I'm a little more familiar with the setup process I might try a full manual
installation.

## Reference

- [Building the Installation Image](https://guix.gnu.org/manual/en/html_node/Building-the-Installation-Image.html#Building-the-Installation-Image)
- [USB Stick and DVD Installation](https://guix.gnu.org/manual/en/html_node/USB-Stick-and-DVD-Installation.html#USB-Stick-and-DVD-Installation)
- [Guix System with full disk encryption](https://libreboot.org/docs/gnulinux/guix_system.html)
- [Mapped Devices](https://guix.gnu.org/manual/en/html_node/Mapped-Devices.html)
- [Guix: A most advanced operating system](https://ambrevar.xyz/guix-advance/)
