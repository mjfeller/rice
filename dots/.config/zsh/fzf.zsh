# A collection of useful fzf wrapper functions

# repeat history
fh() { 
  print -z $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) \
	  | fzf +s --tac \
	  | sed 's/ *[0-9]* *//')
}

# copy history
fhc() {
  echo -n $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) \
	  | fzf +s --tac | sed 's/ *[0-9]* *//') \
	  | pbcopy
}

# kill a process
fkill() { 
  local pid
  pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')

  if [ "x$pid" != "x" ]
  then
    echo $pid | xargs kill -${1:-9}
  fi
}

