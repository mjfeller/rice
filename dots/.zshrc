plugins=(git osx brew)
echo -e "\033]; \007"
source ~/.config/oh-my-zsh/oh-my-zsh.sh

## Alias
alias ls="ls -F"
alias ll="ls -lh"
alias emacs="TERM=xterm-256color emacs"
alias q="exit"
alias neo="clear && neofetch"
alias notify='terminal-notifier -title "Terminal" -message "Done with taks"'
alias weather="curl wttr.in/\~Boulder+Colorado"
alias f="fzf --preview=\"head -$LINES {}\""
alias fz="vim \$(f)"
alias vi=vim
alias yt="youtube-dl"
alias r="ranger"

# Kubernetes
alias kc="kubectl --kubeconfig=$HOME/cluster.config"
alias helm="helm --kubeconfig=$HOME/cluster.config"
alias c="cd $CORE"
alias o="cd $OPS"

## Kubernetes
function gdep {
    godepgraph -o gitlab.com/redeam -s gitlab.com/redeam/core/$1 | dot -Tpng -o /tmp/dep.png
    open /tmp/dep.png
}

alias kt="ktail --kubeconfig=$HOME/cluster.config -t '
$BOLD$LIGHT_CYAN{{ .Container.Name }}$NONE $DIM{{ .Pod.Name }}$NONE $YELLOW{{ if .Timestamp }}{{ .Timestamp.Format \"2006-01-02T15:04:05.999999999Z07:00\" }}{{ end }}$NONE
{{ .Message }} '"

# repeat history
function fh {
  print -z $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed 's/ *[0-9]* *//')
}

function fhc {
  echo -n $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed 's/ *[0-9]* *//') | pbcopy
}

# fkill - kill process
fkill() {
  local pid
  pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')

  if [ "x$pid" != "x" ]
  then
    echo $pid | xargs kill -${1:-9}
  fi
}

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

autoload -U +X bashcompinit && bashcompinit

if [[ "$OSTYPE" == "linux-gnu" ]]; then
    setxkbmap -layout us -option ctrl:nocaps

    # The next line updates PATH for the Google Cloud SDK.
    if [ -f '/home/mjf/prog/google-cloud-sdk/path.zsh.inc' ]; then
        . '/home/mjf/prog/google-cloud-sdk/path.zsh.inc';
    fi

    # The next line enables shell command completion for gcloud.
    if [ -f '/home/mjf/prog/google-cloud-sdk/completion.zsh.inc' ]; then
        . '/home/mjf/prog/google-cloud-sdk/completion.zsh.inc';
    fi

elif [[ "$OSTYPE" == "darwin"* ]]; then
    if [ -f '/Users/markfeller/Downloads/google-cloud-sdk/completion.zsh.inc' ]; then
        source '/Users/markfeller/Downloads/google-cloud-sdk/completion.zsh.inc';
    fi
fi

