#!/bin/sh

PKGS=`grep -v ^\# packages | grep . | awk '{print $1}' | paste -sd ' ' -`

bootstrap_linux() {
    echo "Installing Packages"
    sudo dnf install $PKGS
}

bootstrap_osx() {
    echo "Installing Packages"
    brew install $PKGS
}

if [[ "$OSTYPE" == "linux-gnu" ]]; then
    bootstrap_linux
elif [[ "$OSTYPE" == "darwin"* ]]; then
    bootstrap_osx
    IGNORE="--ignore bin|conkyrc|tmux.conf|dunst|rofi|i3|i3status"
fi

stow $IGNORE dots
