#!/bin/sh

killall intel-virtual-o # Yeah, that's how killall sees the name
intel-virtual-output

xrandr --output VIRTUAL3 --off --output VIRTUAL2 --off --output VIRTUAL1 --primary --mode VIRTUAL1.447-3840x2160 --pos 0x0 --rotate normal --output eDP1 --mode 3840x2160 --pos 3840x0 --rotate normal --output VIRTUAL5 --off --output VIRTUAL4 --off --output VIRTUAL6 --off

# TODO: Split this into a separate script for use in i3
#i3-msg -q "workspace --no-auto-back-and-forth number 1; move workspace to output primary; workspace back_and_forth"
#i3-msg -q "workspace --no-auto-back-and-forth number 2; move workspace to output primary; workspace back_and_forth"
#i3-msg -q "workspace --no-auto-back-and-forth number 3; move workspace to output primary; workspace back_and_forth"

nitrogen --restore
