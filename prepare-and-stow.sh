#!/usr/bin/env sh

mkdir -p ~/.local/share
mkdir -p ~/.local/state
mkdir -p ~/.local/src
mkdir -p ~/.cache
mkdir -p ~/.config

cd $(dirname ${0})
stow -d . -t ~ -R stow
