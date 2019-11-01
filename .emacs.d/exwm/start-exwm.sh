#!/bin/sh

# Disable access control for the current user
xhost +SI:localuser:$USER

# Make Java applications aware this is a non-reparenting window manager
export _JAVA_AWT_WM_NONREPARENTING=1

# Start authentication daemons
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
gpg-connect-agent /bye

# Make Flatpak apps visible to launcher
export XDG_DATA_DIRS="$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share"

# Make sure `ls` collates dotfiles first (for dired)
export LC_COLLATE="C"

# Start Xfce's settings manager
xfsettingsd &

# Start Shepherd to manage user daemons
if [ -z "$(pgrep -u daviwil shepherd)" ]; then
  shepherd
fi

# Enable screen compositing
compton &

# Turn off the system bell
xset -b

# Enable screen locking on suspend
xss-lock -- slock &

# We're in Emacs, yo
export VISUAL=emacsclient
export EDITOR="$VISUAL"

# Fire it up
exec dbus-launch --exit-with-session emacs -mm --use-exwm
