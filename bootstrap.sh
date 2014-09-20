#!/bin/bash
# Basic bootstrapping script for daviwil's dotfiles

function bootstrap_mac {

    # Is Homebrew installed?
    if which brew 2> /dev/null; then
        echo "* Homebrew already installed"
    else
        echo "Can't find Homebrew"
        echo "To install it open a Terminal window and type :"
        echo /usr/bin/ruby -e \"\$\(curl\ \-fsSL\ https\:\/\/raw\.github\.com\/Homebrew\/homebrew\/go\/install\)\"
    fi
}

if [ "$(uname -s)" == "Darwin" ]; then
    # Bootstrap Mac OS X
    echo ""
    echo -e "\033[34mConfiguring for Mac OS X...\033[0m"
    echo ""
    bootstrap_mac

elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    # Bootstrap Linux
    # TODO: Ubuntu vs other distro?a
    echo "TODO: Bootstrap Linux"

elif [ "$(expr substr $(uname -s) 1 6)" == "CYGWIN" ]; then
    # Bootstrap Cygwin
    # TODO: Steps
    # - Set up apt-cyg
    # - Install git, zsh, wget, etc
    # - Build tmux
    echo "TODO: Bootstrap Cygwin"
fi

# Pull github repo

