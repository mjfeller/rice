export PATH=$PATH:$HOME/.local/bin

# User configuration
export TERMINAL=/usr/bin/alacritty
export VISUAL=/usr/bin/vim
export EDITOR=/usr/bin/vim
export PAGER=/usr/bin/less
export SHELL=/usr/bin/zsh
export MANPATH=/usr/local/share/man:$MANPATH
export LESSHST=~/.cache/lesshst

# ZSH Config
export ZDOTDIR=$HOME/.config/zsh
export ZSHDDIR=$HOME/.config/zsh
export ZSH=$HOME/.config/oh-my-zsh
export SHELL_SESSION_HISTORY=0

# Go
export GOPATH=$HOME/prog/go
export PATH=$PATH:$GOPATH/bin

# Rust
export PATH=$PATH:$HOME/.cargo/bin

# fzf
export FZF_DEFAULT_OPTS='--height 40% --exact'
export FZF_CTRL_T_OPTS="--preview '(highlight -O ansi -l {} 2> /dev/null || cat {} || tree -C {}) 2> /dev/null | head -200'"
export FZF_CTRL_R_OPTS='--sort --exact'

export LC_COLLATE="C"
