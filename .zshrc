plugins=(git osx brew)
echo -e "\033]; \007"
source ~/.oh-my-zsh/oh-my-zsh.sh

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

# Kubernetes
alias kc="kubectl --kubeconfig=$HOME/cluster.config"
alias helm="helm --kubeconfig=$HOME/cluster.config"
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

# repeat history
function fh {
  print -z $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed 's/ *[0-9]* *//')
}

function fhc {
  echo -n $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed 's/ *[0-9]* *//') | pbcopy
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

~/.vim/plugged/gruvbox/gruvbox_256palette_osx.sh

# if [ -z $TMUX ]
# then
#         tmux ls && read tmux_session && tmux attach -t ${tmux_session:-default} || tmux new -s ${tmux_session:-default}
# fi

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/markfeller/Downloads/google-cloud-sdk/path.zsh.inc' ]; then source '/Users/markfeller/Downloads/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/markfeller/Downloads/google-cloud-sdk/completion.zsh.inc' ]; then source '/Users/markfeller/Downloads/google-cloud-sdk/completion.zsh.inc'; fi
