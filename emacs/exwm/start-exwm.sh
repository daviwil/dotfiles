#!/bin/sh

# Set XDG_DATA_DIRS because something in Manjaro broke it :/
export XDG_DATA_DIRS="/usr/share:$XDG_DATA_DIRS"

# Start authentication daemons
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 & eval $(gnome-keyring-daemon -s --components=pkcs11,secrets,ssh,gpg) &

# Make things look nice
compton --config ~/.dotfiles/emacs/compton.conf &
nitrogen --restore &

# Start Xfce's settings and power managers
xfsettingsd &
xfce4-power-manager &

# Start up Syncthing for file synchronization
syncthing-gtk --minimized &

# Load system tray apps for sound, bluetooth, and networking
blueman-applet &
pasystray &
nm-applet &

# Enable Manjaro update checks
pamac-tray &

# Enable screen locking on suspend
xss-lock -- slock &

# We're in Emacs, yo
export VISUAL=emacsclient
export EDITOR="$VISUAL"

# Fire it up
emacs --use-exwm
