#!/bin/sh

function link {
	ln -sFf $1 $HOME/$1
}

function link_config {
	link .tmux.conf
	link .zshrc
	link .zshenv

	link .config/mpd
	link .config/ranger
	link .config/neofetch
	link .config/alactritty

	link .scripts
}

function setup_mpd {
	mkdir -p .config/mpd/playlists
	touch .config/mpd/database
	touch .config/mpd/log
	touch .config/mpd/pid
	touch .config/mpd/state
	touch .config/mpd/sticker.sql
}

function setup_vim {
	link .vimrc
	link .vim
}

link_config
setup_mpd
setup_vim
