#!/bin/sh

pkgs() {
    grep -v ^\# $1 | grep . | awk '{print $1}' | paste -sd ' ' -
}

PKGS=`pkgs packages`
LINUX_PKGS=`pkgs packages_linux`
MACOS_PKGS=`pkgs packages_macos`

bootstrap_linux() {
    echo "Installing Packages"
    echo sudo dnf install $PKGS $LINUX_PKGS
}

bootstrap_osx() {
    echo "Installing Packages"
    brew install $PKGS $MACOS_PKGS
}

if [[ "$OSTYPE" == "linux-gnu" ]]; then
    bootstrap_linux
elif [[ "$OSTYPE" == "darwin"* ]]; then
    bootstrap_osx
    IGNORE="--ignore bin|conkyrc|tmux.conf|dunst|rofi|i3|i3status"
fi

echo "Linking Dots"
stow $IGNORE dots
