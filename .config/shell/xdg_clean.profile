#!/bin/sh

export XINITRC=${XDG_CONFIG_HOME:-$HOME/.config}/x11/xinitrc
export HISTFILE=${XDG_STATE_HOME:-$HOME/.local/state}/history
export LESSHISTFILE="-"
export INPUTRC=${XDG_CONFIG_HOME:-$HOME/.config}/shell/inputrc
export ZDOTDIR=${XDG_CONFIG_HOME:-$HOME/.config}/zsh
export WGETRC="${XDG_CONFIG_HOME:-$HOME/.config}/wget/wgetrc"
export VIMINIT='let $MYVIMRC = !has("nvim") ? "$XDG_CONFIG_HOME/vim/vimrc" : "$XDG_CONFIG_HOME/nvim/init.lua" | so $MYVIMRC'
export ZK_NOTEBOOK_DIR="$HOME/notes"
# FZF
export FZF_DEFAULT_OPTS="--reverse --cycle"
export FZF_TMUX=1
# export WINEPREFIX="${XDG_DATA_HOME:-$HOME/.local/share}/wineprefixes/default"
export STARSHIP_CONFIG=${XDG_CONFIG_HOME:-$HOME/.config}/starship/config.toml
export GTK2_RC_FILES="${XDG_CONFIG_HOME:-$HOME/.config}/gtk-2.0/gtkrc-2.0"
# Vagrant
export VAGRANT_HOME="$XDG_DATA_HOME"/vagrant
export VAGRANT_ALIAS_FILE="$XDG_DATA_HOME"/vagrant/aliases
# Node#npm
export NPM_CONFIG_USERCONFIG=${XDG_CONFIG_HOME:-$HOME/.config}/npm/npmrc
export PATH="$PATH:$XDG_DATA_HOME/npm/bin"
# Ruby#gem
export GEM_HOME="$XDG_DATA_HOME"/gem
export GEM_SPEC_CACHE="$XDG_CACHE_HOME"/gem
# Rust#cargo
export CARGO_HOME=${XDG_DATA_HOME:-$HOME/.local/share}/cargo
export RUSTUP_HOME=${XDG_DATA_HOME:-$HOME/.local/share}/rustup
. "$XDG_DATA_HOME/cargo/env"
# GO#go
export GOPATH="$XDG_DATA_HOME"/go
export GOMODCACHE="$XDG_CACHE_HOME"/go/mod
