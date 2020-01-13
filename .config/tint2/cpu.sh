#!/bin/sh
# by Paul Colby (http://colby.id.au), no rights reserved ;)
# Acquired on 1/13/20 from https://github.com/pcolby/scripts/blob/master/cpu.sh

PREV_TOTAL=0
PREV_IDLE=0

while true; do
  # Get the total CPU statistics, discarding the 'cpu ' prefix.
  CPU=($(sed -n 's/^cpu\s//p' /proc/stat))
  IDLE=${CPU[3]} # Just the idle CPU time.

  # Calculate the total CPU time.
  TOTAL=0
  for VALUE in "${CPU[@]:0:8}"; do
    TOTAL=$((TOTAL+VALUE))
  done

  # Calculate the CPU usage since we last checked.
  DIFF_IDLE=$((IDLE-PREV_IDLE))
  DIFF_TOTAL=$((TOTAL-PREV_TOTAL))
  DIFF_USAGE=$(((1000*(DIFF_TOTAL-DIFF_IDLE)/DIFF_TOTAL+5)/10))
  echo -en "CPU: $DIFF_USAGE%\n"

  # Remember the total and idle CPU times for the next check.
  PREV_TOTAL="$TOTAL"
  PREV_IDLE="$IDLE"

  # Wait before checking again.
  sleep 2
done
