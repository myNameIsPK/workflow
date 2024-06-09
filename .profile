#!/bin/sh
## XDG base directory
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
export XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}"

## MY environments vars
export DOTFILES="$HOME/.dotfile.git"
export MY_SHELL="$(ps -p $$ -ocomm=)"
export MY_BIN="$HOME/bin"
export MY_LIB="$HOME/lib"
export MY_SRC="$HOME/.local/src"
export MY_BACKGROUND="light"
export MY_COLOR_CACHE_DIR="$XDG_CACHE_HOME/colorsync"
export MY_NOTES_DIR="$HOME/notes"

export PATH="$HOME/.local/bin:$PATH"
export PATH="$(echo $(find $MY_BIN -type d -printf %p:))$PATH"

## Default program
export TERM="${TERM:-xterm-256color}"
export EDITOR="nvim"
export VISUAL="nvim"
export MANPAGER='nvim +Man!'
# export MANWIDTH=999
export TERMINAL="wezterm start"
export GPG_TTY=$(tty)

browser_list="firefox-nightly firefox brave chromium"
OLDIFS=$IFS
IFS=" "
if [ -n "$ZSH_VERSION" ]; then
    setopt sh_word_split
fi
for i in ${browser_list}; do
    if command -v "$i" > /dev/null; then
        export BROWSER="$i"
        break
    fi
done
IFS=$OLDIFS

## XDG clean up
# Tools
export XINITRC=${XDG_CONFIG_HOME:-$HOME/.config}/x11/xinitrc
export HISTFILE=${XDG_STATE_HOME:-$HOME/.local/state}/history
# export ZDOTDIR=${XDG_CONFIG_HOME:-$HOME/.config}/zsh
export WGETRC="${XDG_CONFIG_HOME:-$HOME/.config}/wget/wgetrc"
# export WINEPREFIX="${XDG_DATA_HOME:-$HOME/.local/share}/wineprefixes/default"
export STARSHIP_CONFIG=${XDG_CONFIG_HOME:-$HOME/.config}/starship/config.toml
export GTK2_RC_FILES="${XDG_CONFIG_HOME:-$HOME/.config}/gtk-2.0/gtkrc-2.0"
# Docker
export DOCKER_CONFIG="$XDG_CONFIG_HOME"/docker
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

## Tools config
export VIMINIT='let $MYVIMRC = !has("nvim") ? "$XDG_CONFIG_HOME/vim/vimrc" : "$XDG_CONFIG_HOME/nvim/init.lua" | so $MYVIMRC'
export ZK_NOTEBOOK_DIR="$MY_NOTES_DIR"
export LESSHISTFILE="-" # stop less from store history
# FZF
export FZF_DEFAULT_OPTS="--reverse --cycle --scroll-off=3"
# export FZF_TMUX=1
# BAT
export BAT_THEME="gruvbox-${MY_BACKGROUND:-dark}"
# Vagrant
export VAGRANT_DEFAULT_PROVIDER=libvirt

## Programing Languages
# Python#pip
# export PIP_REQUIRE_VIRTUALENV=true
# Node#npm
export PATH="$PATH:$XDG_DATA_HOME/npm/bin"
# Bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"
# Rust#cargo
if [ -f "$XDG_DATA_HOME/cargo/env" ]; then
    . "$XDG_DATA_HOME/cargo/env"
fi

test -f $HOME/.profile.local && . $HOME/.profile.local

## Guix <https://guix.gnu.org/manual/en/html_node/Invoking-guix-package.html>
GUIX_PROFILE="$HOME/.guix-profile" ; \
test -f "$GUIX_PROFILE/etc/profile" && source "$GUIX_PROFILE/etc/profile"

# When user X11 server
startx_y_or_n() {
  trap break SIGINT
  # exec startx
  while true; do
    printf "Do you want to start X server? (y/n): "
    read yn
    case $yn in
      [Yy]*) startx "$XINITRC" && break ;;
      [Nn]*) break ;;
      *) echo "(y/n) only";;
    esac
  done
  trap - SIGINT
}

if [ -z "${DISPLAY}" ] && [ "${XDG_VTNR}" -eq 1 ]; then startx_y_or_n; fi

## End of file
