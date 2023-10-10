#!/bin/bash

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# default PS1='[\u@\h \W]\$ '
rst="\[$(tput sgr0)\]"
# blk="\[$(tput setaf 0)\]"
red="\[$(tput setaf 1)\]"
gre="\[$(tput setaf 2)\]"
# yel="\[$(tput setaf 3)\]"
blu="\[$(tput setaf 4)\]"
# mgt="\[$(tput setaf 5)\]"
# cya="\[$(tput setaf 6)\]"
# whi="\[$(tput setaf 7)\]"
PS1="${red}[${rst}\u${red}@${rst}\h ${blu}\W${red}]${gre}\$${rst} "
# GIT_PS1_SHOWDIRTYSTATE=1
# GIT_PS1_SHOWSTASHSTATE=1
# GIT_PS1_SHOWUNTRACKEDFILES=1
# GIT_PS1_SHOWUPSTREAM=1
# GIT_PS1_DESCRIBE_STYLE=1
# GIT_PS1_SHOWCOLORHINTS=1
# . /usr/share/git/completion/git-prompt.sh
# PS1+='$(__git_ps1 "(%s) ")'

# Load aliases
shortcuts-gen > /dev/null 2>&1
[[ -f ${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliases ]] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliases"

HISTCONTROL=ignoreboth	# Ignore duplicate in history 

stty stop undef		# Disable ctrl-s to freeze terminal.

## SHOPT
shopt -s autocd		# Auto cd to directory
shopt -s cdspell	# Auto correct dirctory name
shopt -s cmdhist	# Save multi-line commands in history as single
shopt -s dotglob	# Includ dot file in globbling
shopt -s histappend	# Append history not overwrite when exit
shopt -s checkwinsize	# Checks term size when bash regains control

[ -f "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf/fzf.bash ] && source "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf/fzf.bash

# Automatically added by the Guix install script.
if [ -n "$GUIX_ENVIRONMENT" ]; then
    if [[ $PS1 =~ (.*)"\\$" ]]; then
        PS1="${BASH_REMATCH[1]} [env]\\\$ "
    fi
fi

