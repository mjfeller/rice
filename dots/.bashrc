# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi

# User specific environment
PATH="$HOME/.local/bin:$HOME/.bin:$PATH"
export PATH

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions

## Prompt
export PS1="\e[37mλ \W »\e[0m "

## Alias
alias ls="ls -F"
alias ll="ls -lh"
alias l="ls -la"
alias emacs="TERM=xterm-256color emacs"
alias q="exit"
alias :q="exit"
alias neo="clear && neofetch"
alias notify='terminal-notifier -title "Terminal" -message "Done with taks"'
alias weather="curl wttr.in/\~Boulder+Colorado"
alias f="fzf --preview=\"head -$LINES {}\""
alias fz="vim \$(f)"
alias vi=vim
alias yt="youtube-dl"

# Kubernetes
alias kc="kubectl --kubeconfig=$HOME/cluster.config"
alias helm="helm --kubeconfig=$HOME/cluster.config"
export CORE=$GOPATH/src/gitlab.com/redeam/core
alias c="cd $CORE"

## Kubernetes
function gdep {
    godepgraph -o gitlab.com/redeam -s gitlab.com/redeam/core/$1 | dot -Tpng -o /tmp/dep.png
    open /tmp/dep.png
}

function gp {
  kubectl --kubeconfig=$HOME/cluster.config get pods | egrep -o  "$1.*Running" | egrep -o "$1[^ ]*" | head -1
}

function kcforward {
  POD=`gp $1`
  kubectl --kubeconfig=$HOME/cluster.config port-forward $POD $2:8001
}

alias kt="ktail --kubeconfig=$HOME/cluster.config -t '
$BOLD$LIGHT_CYAN{{ .Container.Name }}$NONE $DIM{{ .Pod.Name }}$NONE $YELLOW{{ if .Timestamp }}{{ .Timestamp.Format \"2006-01-02T15:04:05.999999999Z07:00\" }}{{ end }}$NONE
{{ .Message }} '"

# coloured manuals
man() {
  env \
    LESS_TERMCAP_mb=$(printf "\e[1;31m") \
    LESS_TERMCAP_md=$(printf "\e[1;31m") \
    LESS_TERMCAP_me=$(printf "\e[0m") \
    LESS_TERMCAP_se=$(printf "\e[0m") \
    LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
    LESS_TERMCAP_ue=$(printf "\e[0m") \
    LESS_TERMCAP_us=$(printf "\e[1;32m") \
    man "$@"
}

setxkbmap -layout us -option ctrl:nocaps

# fkill - kill process
fkill() {
  local pid
  pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')

  if [ "x$pid" != "x" ]
  then
    echo $pid | xargs kill -${1:-9}
  fi
}

# ----------------------------------------------------------------------
# Env
export TERMINAL=$HOME/.bin/alacritty
export PATH=$PATH:/usr/local/bin
export PATH=$PATH:$HOME/.bin

export HISTSIZE=1000
export SAVEHIST=1000
export HISTFILE=~/.history

## User configuration
export MANPATH=/usr/local/share/man:$MANPATH

## Haskel
export PATH=$PATH:$HOME/Library/Haskell/bin

## Go
export GOPATH=$HOME/prog/go
export PATH=$PATH:$GOPATH/bin

## Rust
export PATH=$PATH:$HOME/.cargo/bin

## fzf
export FZF_DEFAULT_OPTS='--height 40% --exact'
export FZF_CTRL_T_OPTS="--preview '(highlight -O ansi -l {} 2> /dev/null || cat {} || tree -C {}) 2> /dev/null | head -200'"
export FZF_CTRL_R_OPTS='--sort --exact'

## Google Cloud SDK
export PATH="/usr/local/opt/texinfo/bin:$PATH"
export PATH="/usr/local/opt/gpg-agent/bin:$PATH"




# re-wrote the script above
bind '"\C-r": "\C-x1\e^\er"'
bind -x '"\C-x1": __fzf_history';

__fzf_history ()
{
__ehc $(history | fzf --tac --tiebreak=index | perl -ne 'm/^\s*([0-9]+)/ and print "!$1"')
}

__ehc()
{
if
        [[ -n $1 ]]
then
        bind '"\er": redraw-current-line'
        bind '"\e^": magic-space'
        READLINE_LINE=${READLINE_LINE:+${READLINE_LINE:0:READLINE_POINT}}${1}${READLINE_LINE:+${READLINE_LINE:READLINE_POINT}}
        READLINE_POINT=$(( READLINE_POINT + ${#1} ))
else
        bind '"\er":'
        bind '"\e^":'
fi
}

# auto command to cd without cd
shopt -s autocd
