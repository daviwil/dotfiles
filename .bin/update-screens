#!/bin/sh

case $(hostname) in

    zerocool)
        xrandr --output VIRTUAL1 --off --output eDP1 --mode 2560x1440 --pos 3840x416 --rotate normal --output DP1 --off --output HDMI1 --off --output DP1-3 --off --output DP1-2 --off --output DP1-1 --primary --mode 3840x2160 --pos 0x0 --rotate normal --output DP2 --off
        ;;

    davinci)
        # Temporary: this is for docking my laptop at home with HDMI!
        #xrandr --output HDMI-2 --mode 3840x2160 --pos 0x0 --scale 0.6x0.6 --primary --rotate normal --output HDMI-1 --off --output DP-1 --off --output eDP-1 --mode 1920x1080 --pos 2304x216 --rotate normal --output DP-2 --off
        xrandr --output eDP-1 --mode 1920x1080 --pos 2560x360 --rotate normal --output DP-1-2 --primary --mode 2560x1440 --pos 0x0 --rotate normal --output HDMI-2 --off --output HDMI-1 --off --output DP-1 --off --output DP-1-3 --off --output DP-2 --off --output DP-1-1 --off
        ;;

    phantom)
        killall intel-virtual-o # Yeah, that's how killall sees the name
        intel-virtual-output

        xrandr --output VIRTUAL3 --off --output VIRTUAL2 --off --output VIRTUAL1 --primary --mode VIRTUAL1.447-3840x2160 --pos 0x0 --rotate normal --output eDP1 --mode 3840x2160 --pos 3840x0 --rotate normal --output VIRTUAL5 --off --output VIRTUAL4 --off --output VIRTUAL6 --off
        ;;

esac
