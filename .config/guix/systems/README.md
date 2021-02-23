# Guix Configuration

## Deprecation Notice

I've rewritten my installation structions in Org Mode so this file is now out of date.  You can check out the updated steps in my [Systems.org](https://github.com/daviwil/dotfiles/blob/master/Systems.org#system-installation) file.

## Building the Installation Image

Since I use modern Thinkpads, I have to use the non-free kernel and firmware
blobs from the [`nonguix`](https://gitlab.com/nonguix/nonguix) channel.  After
cloning the repo, the installation image can be built with this command:

```shell
guix system disk-image ./install.scm
```

**NOTE:** It can take an hour or more for this to complete, so be patient...

Once the build is complete, you can write the output image to a USB stick:

```shell
sudo dd if=/gnu/store/nyg6jv3a4l0pbcvb0x7jfsb60k9qalga-disk-image of=/dev/sdX status=progress
```

## Installing Guix

With the newly "burned" installation image, boot from the USB drive and choose
"Install using the shell based process".

### Setting up WiFi

First, run `rfkill unblock all` to make sure the wifi card is usable, then run
`ifconfig -a` to determine the name of the wifi interface.  Edit a new file
named `wifi.conf` to configure details about your network:

```
network={
  ssid="ssid-name"
  key_mgmt=WPA-PSK
  psk="unencrypted passphrase"
}
```

Then run `wpa_supplicant -c wifi.conf -i wlp4s0 -B` to connect.

> NOTE: If for any reason running `wpa_supplicant` fails, make sure to kill any
> background instances of it before trying to run it again because the old
> instances will block new runs from working.  This wasted a couple hours of my
> time the first time I tried installing Guix!

The last step to set up networking is to run `dhclient -v wlp4s0` to turn on DNS
for your wifi connection.

### Setting up Partitions

Since we're installing on a Thinkpad with UEFI, there's already an EFI
partition.

If you haven't created your main Linux partition yet, go ahead and do it now
following the [instructions in the Guix
manual](https://guix.gnu.org/manual/en/guix.html#Disk-Partitioning).

Once you have your Linux root partition set up, you can enable LUKS to encrypt
that partition by running the following commands (where `/dev/nvme0n1p5` is your
root partition and `system-root` is an arbitrary label you'd like to use for
it):

```
cryptsetup luksFormat /dev/nvme0n1p5
cryptsetup open --type luks /dev/nvme0n1p5 system-root
mkfs.ext4 -L system-root /dev/mapper/system-root
mount LABEL=system-root /mnt
```

Finally, make sure to mount your EFI partition to `/mnt/boot` so that the
installer can install the bootloader.  The Guix installation instructions
obscure this step slightly so it's easy to miss:

```
mkdir -p /mnt/boot/efi
mount /dev/<EFI partition> /mnt/boot/efi
```

Now your EFI and encrypted root filesystems are mounted so you can proceed with
system installation.

### Initial System Installation

If you've got a system configuration prepared already, you can use `git` to pull
it down into the current directory (the one you're already in, not `/mnt`):

```
git clone https://github.com/daviwil/dotfiles
```

One important step before you attempt system installation is to set up the
`nonguix` channel so that the system can be installed from it.  Once you've
cloned your dotfiles repo, you can place your `channels.scm` file into the root
user's `.config` path and then run `guix pull` to activate it:

```
mkdir -p ~/.config/guix
cp dotfiles/guix/channels.scm ~/.config/guix
guix pull
hash guix  # This is necessary to ensure the updated profile path is active!
```

The pull operation may take a while depending on how recently you generated your
installation USB image (if packages in the main Guix repository have been
updated since then).

Once your channels are set up, you will need to tweak your configuration to
reflect the partition UUIDs and labels for the system that you are installing.
To figure out the UUID of your encrypted root partition, you can use `cryptsetup
luksUUID /dev/<root partition>`.

> **TIP:** To make it easier to copy the UUID into your config file, you can
> switch to another tty using `Fn-Ctrl-Alt-F4` and press `Enter` to get to
> another root prompt.  You can then switch back and forth between the previous
> tty on `F3`.

Once your system configuration is correct, start up `herd`:

```
herd start cow-store /mnt
```

Now you can initialize your system using the following command:

```
guix system init path/to/config.scm /mnt
```

This could take a while, so make sure your laptop is plugged in and let it run.
If you see any errors during installation, don't fret, you can usually resume
from where you left off because your Guix store will have any packages that were
already installed.

## Initial System Setup

Congrats!  You now have a new Guix system installed, reboot now to complete the
initial setup of your user account.

The first thing you'll want to do when you land at the login prompt is login as
`root` and immediately change the password using `passwd` (there isn't a root
password by default!).  Once you do that, change the password for your user
account also using `passwd <username>`.  Now you can log out of the `root`
account and log back in as yourself.

Since we used the `nonguix` channel to install the non-free Linux kernel, we'll
need to make sure that channel is configured in our user account so that we have
access to those packages the next time we `guix pull`.  At the moment I just
symlink my Guix config folder under `~/.config`:

```
ln -sf ~/.dotfiles/guix ~/.config/guix
```

Verify that your `channels.scm` file is in the target path and then run `guix
pull` to sync in the new channel.

Now you can install the packages that you want to use for day-to-day activities.
I like to keep this list of packages in a [manifest
file](../manifests/desktop.scm) so that I can install them all at once like
this:

```
guix package -m ~/.dotfiles/guix/manifests/desktop.scm
```

You can edit this file and then re-run that command any time to reflect the
updates you made.

## Reference

- [Building the Installation Image](https://guix.gnu.org/manual/en/html_node/Building-the-Installation-Image.html#Building-the-Installation-Image)
- [USB Stick and DVD Installation](https://guix.gnu.org/manual/en/html_node/USB-Stick-and-DVD-Installation.html#USB-Stick-and-DVD-Installation)
- [Guix System with full disk encryption](https://libreboot.org/docs/gnulinux/guix_system.html)
- [Mapped Devices](https://guix.gnu.org/manual/en/html_node/Mapped-Devices.html)
- [Guix: A most advanced operating system](https://ambrevar.xyz/guix-advance/)
