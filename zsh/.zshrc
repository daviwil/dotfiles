# Include antigen plugin manager and reference some packages
source ~/.dotfiles/zsh/antigen/antigen.zsh
antigen bundles <<BUNDLES
    git    
    gitignore
    tmux
    vagrant
    docker
    taskwarrior
    brew
    osx 
    redis-cli
    rsync
    mosh
    zsh-users/zsh-syntax-highlighting
    sharat87/zsh-vim-mode
BUNDLES
antigen use oh-my-zsh
antigen theme agnoster
antigen apply

# Set up the prompt
autoload -U promptinit
promptinit
setopt PROMPT_SUBST     # Enable prompt text substitution

# Set up command history
HISTFILE=~/.zsh_history
setopt HIST_VERIFY              # Verify history reuse last command
setopt INC_APPEND_HISTORY       # Add commands to history as they're typed
setopt HIST_IGNORE_ALL_DUPS     # Ignore duplicate history entries
setopt EXTENDED_HISTORY         # Timestamp all history entries
setopt SHARE_HISTORY            # Share history between any running zsh instances
setopt HIST_REDUCE_BLANKS       # Remove extra whitespace from history entries

# Path aliases
# TODO: Fix for Mac!
#alias ls="ls --color=auto"
alias ll="ls -alGFh"
alias llg="ll | grep"
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias mkdir="mkdir -pv"

# Dotfile aliases
alias relshrc="source ~/.dotfiles/zsh/.zshrc"
alias edshrc="vim ~/.dotfiles/zsh/.zshrc && relshrc"
alias dotzsh="cd ~/.dotfiles/zsh/"
alias dotvim="cd ~/.dotfiles/vim/"
alias dotfiles="cd ~/.dotfiles"

# Turn on autocd to go directly to typed directories
setopt autocd

# Turn on VIM bindings for ZLE
bindkey -v

# Path hashes
hash -d codeproj=$HOME/Projects/Code

# Insert a newline after each command
precmd() { print "" }

