compdef r='rcmd'
alias r=rcmd
rcmd() { if [[ $1 == "get" ]]; then shift; command rcmd list $@; else command rcmd $@; fi }

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"

# Recurly
alias rl="cd $GOPATH/src/github.com/recurly"
alias grcmd="$GOPATH/bin/rcmd"
