export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_HOME=$HOME/.local/share
export XDG_CACHE_HOME=$HOME/.cache

export PATH=$PATH:$HOME/.local/bin
export PATH=/usr/local/bin:$PATH

# User configuration
export TERMINAL=alacritty
export VISUAL=vim
export EDITOR=vim
export PAGER=less
export SHELL=zsh
export MANPATH=$MANPATH:/usr/local/share/
export MANWIDTH=72

export LESSHISTFILE=$XDG_CACHE_HOME/lesshst
export HISTFILE=$XDG_CACHE_HOME/bash_history

# ZSH Config
export ZDOTDIR=$XDG_CONFIG_HOME/zsh
export ZSHDDIR=$XDG_CONFIG_HOME/zsh
export ZSH=$XDG_CONFIG_HOME/oh-my-zsh

# Go
export GOPATH=$HOME/prog/go
export PATH=$PATH:$GOPATH/bin

# Rust
export PATH=$PATH:$HOME/.cargo/bin

export LC_COLLATE="C"
