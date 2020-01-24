#!/bin/sh
status="$(playerctl -p spotify status 2>&1)"
if [ "$status" != "No players found" ]
then
  artist="$(playerctl -p spotify metadata artist)"
  if [ "$artist" != "" ]
  then
    echo " $(playerctl -p spotify metadata artist) - $(playerctl -p spotify metadata title)"
  else
    # Clear any string that was previously displayed
    echo ""
  fi
else
  # Clear any string that was previously displayed
  echo ""
fi
