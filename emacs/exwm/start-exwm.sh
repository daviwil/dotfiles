#!/bin/sh

# Disable access control for the current user
xhost +SI:localuser:$USER

# Make Java applications aware this is a non-reparenting window manager
export _JAVA_AWT_WM_NONREPARENTING=1

# Set XDG_DATA_DIRS because something in Manjaro broke it :/
#export XDG_DATA_DIRS="/usr/share:$XDG_DATA_DIRS"

# Start authentication daemons
# TODO: This will be unnecessary once I switch to a GPG subkey for SSH auth
eval $(gnome-keyring-daemon -s --components=pkcs11,secrets,ssh,gpg) &
export SSH_AUTH_SOCK

# Make things look nice
compton &
nitrogen --restore &

# Start the PulseAudio daemon to run persistently
#pulseaudio --daemonize

# Start Xfce's settings manager
xfsettingsd &

# Turn off the system bell
xset -b

# Remap caps lock to ctrl
setxkbmap -layout us -option ctrl:nocaps

# Load system tray apps
nm-applet &

# Enable Manjaro update checks
#pamac-tray &

# Enable screen locking on suspend
xss-lock -- slock &

# We're in Emacs, yo
export VISUAL=emacsclient
export EDITOR="$VISUAL"

# Fire it up
exec dbus-launch --exit-with-session emacs -mm --use-exwm
