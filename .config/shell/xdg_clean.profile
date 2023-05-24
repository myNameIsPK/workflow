#!/bin/sh
# XDG clean up

## Tools
export XINITRC=${XDG_CONFIG_HOME:-$HOME/.config}/x11/xinitrc
export HISTFILE=${XDG_STATE_HOME:-$HOME/.local/state}/history
export INPUTRC=${XDG_CONFIG_HOME:-$HOME/.config}/shell/inputrc
export ZDOTDIR=${XDG_CONFIG_HOME:-$HOME/.config}/zsh
export WGETRC="${XDG_CONFIG_HOME:-$HOME/.config}/wget/wgetrc"
# export WINEPREFIX="${XDG_DATA_HOME:-$HOME/.local/share}/wineprefixes/default"
export STARSHIP_CONFIG=${XDG_CONFIG_HOME:-$HOME/.config}/starship/config.toml
export GTK2_RC_FILES="${XDG_CONFIG_HOME:-$HOME/.config}/gtk-2.0/gtkrc-2.0"
# Vagrant
export VAGRANT_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"/vagrant
export VAGRANT_ALIAS_FILE="${XDG_CONFIG_HOME:-$HOME/.config}"/vagrant/aliases
# Ansible
export ANSIBLE_HOME="${XDG_CONFIG_HOME}/ansible"
export ANSIBLE_CONFIG="${XDG_CONFIG_HOME}/ansible.cfg"
export ANSIBLE_GALAXY_CACHE_DIR="${XDG_CACHE_HOME}/ansible/galaxy_cache"

## Programmint languages
# Python#pyenv
export PYENV_ROOT=$XDG_DATA_HOME/pyenv
# Node#npm
export NPM_CONFIG_USERCONFIG=${XDG_CONFIG_HOME:-$HOME/.config}/npm/npmrc
# Ruby#gem
export GEM_HOME="$XDG_DATA_HOME"/gem
export GEM_SPEC_CACHE="$XDG_CACHE_HOME"/gem
# Rust#cargo
export CARGO_HOME=${XDG_DATA_HOME:-$HOME/.local/share}/cargo
export RUSTUP_HOME=${XDG_DATA_HOME:-$HOME/.local/share}/rustup
# GO#go
export GOPATH="$XDG_DATA_HOME"/go
export GOMODCACHE="$XDG_CACHE_HOME"/go/mod
