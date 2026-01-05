#!/bin/sh
# Creates a 2x2 grid of terminals on workspace 1

TERM_CMD="foot -f 'JetBrains Mono:size=11'"

# Switch to workspace 1 and set split layout
swaymsg "workspace 1; layout splith"

# Launch first terminal (top-left)
$TERM_CMD &
sleep 0.3

# Split horizontally and launch second terminal (top-right)
swaymsg "splith"
$TERM_CMD &
sleep 0.3

# Split vertically and launch third terminal (bottom-right)
swaymsg "splitv"
$TERM_CMD &
sleep 0.3

# Move to top-left, split vertically, launch fourth terminal (bottom-left)
swaymsg "focus left; splitv"
$TERM_CMD &
sleep 0.3

# Focus the top-left terminal
swaymsg "focus up"
