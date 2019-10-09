#!/bin/sh
xrandr --output eDP-1 --mode 2560x1440 --pos 3456x272 --rotate normal --output DP-2 --off --output DP-1 --off --output HDMI-1 --off --output DP-1-3 --off --output DP-1-2 --primary --mode 2560x1440 --pos 0x0 --scale 1.35x1.35 --rotate normal --output DP-1-1 --off

# Make sure the keyboard has capslock configured correctly
setxkbmap -layout us -option ctrl:nocaps
