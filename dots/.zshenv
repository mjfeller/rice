export TERMINAL=$HOME/bin/alacritty
export PATH=$PATH:/usr/local/bin
export PATH=$PATH:$HOME/.local/bin

export VISUAL=vim
export EDITOR=vim
export PAGER=less
export SHELL=zsh

## ZSH Config
export ZDOTDIR=$HOME/.config/zsh
export ZSHDDIR=$HOME/.config/zsh
export ZSH=$HOME/.config/oh-my-zsh
export SHELL_SESSION_HISTORY=0

export HISTSIZE=1000
export SAVEHIST=1000
export HISTFILE=~/.history
export LESSHST=~/.cache/lesshst

export ZSH_THEME="mark"
export DISABLE_LS_COLORS="false"
export DISABLE_AUTO_TITLE="true"

## User configuration
export SHELL=/bin/zsh
export MANPATH=/usr/local/share/man:$MANPATH

## Homebrew
export HOMEBREW_GITHUB_API_TOKEN="30e5da2b006d5f92e5474ff17c7eefe5cad20ee9"
export PATH="/usr/local/sbin:$PATH"

## Haskel
export PATH=$PATH:$HOME/Library/Haskell/bin

## Go
#export GOROOT=/usr/local/go
export GOPATH=$HOME/prog/go
export PATH=$PATH:$GOPATH/bin

## Docker
export DOCKER_HIDE_LEGACY_COMMANDS=1

## Rust
export PATH=$PATH:$HOME/.cargo/bin

## fzf
export FZF_DEFAULT_OPTS='--height 40% --exact'
export FZF_CTRL_T_OPTS="--preview '(highlight -O ansi -l {} 2> /dev/null || cat {} || tree -C {}) 2> /dev/null | head -200'"
export FZF_CTRL_R_OPTS='--sort --exact'

## Google Cloud SDK
export PATH="/usr/local/opt/texinfo/bin:$PATH"
export PATH="/usr/local/opt/gpg-agent/bin:$PATH"

export LC_COLLATE="C"
