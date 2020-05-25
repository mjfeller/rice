#!/bin/zsh
#
# zsh profile file. Runs on login. Environment variables are set here.

export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_HOME=$HOME/.local/share
export XDG_CACHE_HOME=$HOME/.cache

export PATH=$PATH:$HOME/.local/bin/
export PATH=$PATH:/usr/local/bin

export EDITOR=nvim
export READER=zathura
export SHELL=zsh
export TERMINAL=st
export PAGER=less

export MANWIDTH=80
export LESSHISTFILE=-
export HISTFILE=$XDG_CACHE_HOME/bash_history

# ZSH Config
export ZDOTDIR=$XDG_CONFIG_HOME/zsh
export ZSHDDIR=$XDG_CONFIG_HOME/zsh
export ZSH=$XDG_CONFIG_HOME/oh-my-zsh

export GOPATH=$HOME/prog/go
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:$HOME/.cargo/bin

export LS_COLORS="rs=0:di=36;36:ln=36;51:mh=00:pi=40;36;11:so=36;13:do=36;5:bd=48;236;36;11:cd=48;236;36;3:or=48;236;36;9:mi=01;36;41:su=48;196;36;15:sg=48;11;36;16:ca=48;196;36;226:tw=48;10;36;16:ow=48;10;36;21:st=48;21;36;15:ex=1:*.tar=36;9:*.tgz=36;9:*.arc=36;9:*.arj=36;9:*.taz=36;9:*.lha=36;9:*.lz4=36;9:*.lzh=36;9:*.lzma=36;9:*.tlz=36;9:*.txz=36;9:*.tzo=36;9:*.t7z=36;9:*.zip=36;9:*.z=36;9:*.dz=36;9:*.gz=36;9:*.lrz=36;9:*.lz=36;9:*.lzo=36;9:*.xz=36;9:*.zst=36;9:*.tzst=36;9:*.bz2=36;9:*.bz=36;9:*.tbz=36;9:*.tbz2=36;9:*.tz=36;9:*.deb=36;9:*.rpm=36;9:*.jar=36;9:*.war=36;9:*.ear=36;9:*.sar=36;9:*.rar=36;9:*.alz=36;9:*.ace=36;9:*.zoo=36;9:*.cpio=36;9:*.7z=36;9:*.rz=36;9:*.cab=36;9:*.wim=36;9:*.swm=36;9:*.dwm=36;9:*.esd=36;9:*.jpg=36;13:*.jpeg=36;13:*.mjpg=36;13:*.mjpeg=36;13:*.gif=36;13:*.bmp=36;13:*.pbm=36;13:*.pgm=36;13:*.ppm=36;13:*.tga=36;13:*.xbm=36;13:*.xpm=36;13:*.tif=36;13:*.tiff=36;13:*.png=36;13:*.svg=36;13:*.svgz=36;13:*.mng=36;13:*.pcx=36;13:*.mov=36;13:*.mpg=36;13:*.mpeg=36;13:*.m2v=36;13:*.mkv=36;13:*.webm=36;13:*.ogm=36;13:*.mp4=36;13:*.m4v=36;13:*.mp4v=36;13:*.vob=36;13:*.qt=36;13:*.nuv=36;13:*.wmv=36;13:*.asf=36;13:*.rm=36;13:*.rmvb=36;13:*.flc=36;13:*.avi=36;13:*.fli=36;13:*.flv=36;13:*.gl=36;13:*.dl=36;13:*.xcf=36;13:*.xwd=36;13:*.yuv=36;13:*.cgm=36;13:*.emf=36;13:*.ogv=36;13:*.ogx=36;13:*.aac=36;45:*.au=36;45:*.flac=36;45:*.m4a=36;45:*.mid=36;45:*.midi=36;45:*.mka=36;45:*.mp3=36;45:*.mpc=36;45:*.ogg=36;45:*.ra=36;45:*.wav=36;45:*.oga=36;45:*.opus=36;45:*.spx=36;45:*.xspf=36;45:"

export LC_ALL=C
