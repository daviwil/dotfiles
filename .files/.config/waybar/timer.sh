#!/bin/sh

emacsclient -e "(dw/waybar-timer-status)" | sed 's/\"//g'
