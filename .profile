# Augment PATH
export PATH="$HOME/.bin:$HOME/.npm-global/bin:$PATH"

# Make sure we can reach the GPG agent for SSH auth
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

# Make sure `ls` collates dotfiles first (for dired)
export LC_COLLATE="C"

# Make Flatpak apps visible to launcher
export XDG_DATA_DIRS="$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share"

# We're in Emacs, yo
export VISUAL=emacsclient
export EDITOR="$VISUAL"

# Add Azure CLI to PATH if it exists
if [ -f $HOME/Tools/azure-cli ]; then
  export PATH=$PATH:$HOME/Tools/azure-cli/bin
  source '$HOME/Tools/azure-cli/az.completion'
fi

# Load .bashrc to get login environment
[ -f ~/.bashrc ] && . ~/.bashrc
