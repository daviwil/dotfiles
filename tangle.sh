#!/usr/bin/env bash
# Script to tangle Emacs.org configuration file

cd "$(dirname "$0")"

echo "Tangling Emacs.org configuration..."
emacs -Q --batch \
  --eval "(require 'ob-tangle)" \
  --eval "(org-babel-tangle-file \"Emacs.org\")" \
  --kill

echo "Configuration tangled successfully!"