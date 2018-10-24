#!/bin/sh
xrandr --output VIRTUAL1 --off --output eDP1 --mode 2560x1440 --pos 3840x416 --rotate normal --output DP1 --off --output HDMI1 --off --output DP1-3 --off --output DP1-2 --off --output DP1-1 --primary --mode 3840x2160 --pos 0x0 --rotate normal --output DP2 --off

i3-msg -q "workspace --no-auto-back-and-forth number 1; move workspace to output primary; workspace back_and_forth"
i3-msg -q "workspace --no-auto-back-and-forth number 2; move workspace to output primary; workspace back_and_forth"
i3-msg -q "workspace --no-auto-back-and-forth number 3; move workspace to output primary; workspace back_and_forth"
i3-msg -q "workspace --no-auto-back-and-forth number 5; move workspace to output primary; workspace back_and_forth"

nitrogen --restore
