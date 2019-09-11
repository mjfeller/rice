plugins=(git osx brew)
echo -e "\033]; \007"
source ~/.config/oh-my-zsh/oh-my-zsh.sh
source ~/.config/aliases

gdep() {
    godepgraph -o gitlab.com/redeam -s gitlab.com/redeam/core/$1 | dot -Tpng -o /tmp/dep.png
    open /tmp/dep.png
}

fh() { # repeat history
  print -z $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) \
	  | fzf +s --tac \
	  | sed 's/ *[0-9]* *//')
}

fhc() { # copy history
  echo -n $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) \
	  | fzf +s --tac | sed 's/ *[0-9]* *//') \
	  | pbcopy
}

fkill() { # kill a process
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


# ANSI 8-bit Format Example:
#   di=01;38;5;12
# Explanation: 
#   01      bold
#   38;5    use 8-bit code 
#   12      8-bit code
# 
# Colour map: 
# human    3-bit   8-bit 
# blue      34      12 
# cyan      36      14 
# green     32      10 

LS_COLORS='rs=0:di=01;38;5;12:ln=01;38;5;14:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:mi=00:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=38;5;12;42:st=37;44:ex=01;38;5;10:'
export LS_COLORS
