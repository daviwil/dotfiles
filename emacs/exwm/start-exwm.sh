#!/bin/sh

/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 & eval $(gnome-keyring-daemon -s --components=pkcs11,secrets,ssh,gpg) &
compton --config ~/.dotfiles/emacs/compton.conf &
nitrogen --restore &
xfsettingsd &
xfce4-power-manager &
pasystray &
syncthing-gtk -m &
blueman-applet &
pamac-tray &

export VISUAL=emacsclient
export EDITOR="$VISUAL"

emacs
