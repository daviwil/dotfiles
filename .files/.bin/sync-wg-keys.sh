#!/usr/bin/env bash
set -e

# Get current hostname
HOSTNAME=$(hostname)

echo "Syncing WireGuard keys for $HOSTNAME..."

# Create directory if it doesn't exist
sudo mkdir -p /var/lib/wireguard
sudo chmod 700 /var/lib/wireguard

# Export key from pass using hostname but save to generic path
if pass show "wireguard/$HOSTNAME-private" >/dev/null 2>&1; then
    pass show "wireguard/$HOSTNAME-private" | sudo tee "/var/lib/wireguard/private-key" > /dev/null
    sudo chmod 600 "/var/lib/wireguard/private-key"
    sudo chown root:root "/var/lib/wireguard/private-key"
    echo "WireGuard key for $HOSTNAME synced to /var/lib/wireguard/private-key"
else
    echo "Error: No WireGuard key found in pass for 'wireguard/$HOSTNAME-private'"
    echo "Please store the key with: pass insert wireguard/$HOSTNAME-private"
    exit 1
fi