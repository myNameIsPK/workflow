#!/bin/zsh

# default PS1='[%n@%M %c]\$ '
rst="%{$(tput sgr0)%}"
# blk="%{$(tput setaf 0)%}"
red="%{$(tput setaf 1)%}"
gre="%{$(tput setaf 2)%}"
# yel="%{$(tput setaf 3)%}"
blu="%{$(tput setaf 4)%}"
# mgt="%{$(tput setaf 5)%}"
# cya="%{$(tput setaf 6)%}"
# whi="%{$(tput setaf 7)%}"
exitcolor() { if [[ $? == 0 ]]; then echo "${gre}"; else echo "${red}"; fi }
PS1="${red}[${rst}%n${red}@${rst}%M ${blu}%c${red}]\$(exitcolor)\$${rst}"
[ -f "$ZDOTDIR/zsh-git-prompt" ] && source "$ZDOTDIR/zsh-git-prompt"
PS1+="%b "

# Load aliases
shortcuts-gen > /dev/null 2>&1
# [ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/profile" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/profile"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliases" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliases"

# Move history in xdg-cache
HISTSIZE=10000000
SAVEHIST=10000000
# HISTFILE=$HOME/.local/state/bash/history

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

## SET VI MODE ###
bindkey -v
export KEYTIMEOUT=0

stty stop undef # Disable ctrl-s to freeze terminal.

## SETOPT
unsetopt beep # disable bell-noise
setopt autocd # Automatically cd into typed directory.
setopt interactive_comments
setopt histignoredups # Ignore duplicate in history
setopt incappendhistory # append entered command to history, don't wait for shell exit
setopt histignorespace # ignore command start with whitespace
setopt correctall
setopt combining_chars # fix unicode
setopt auto_pushd # Push the current directory visited on the stack.
setopt pushd_ignore_dups # Do not store duplicates in the stack.
# setopt pushd_silent # Do not print the directory stack after pushd or popd.

## Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select

# Ignore case when auto complete
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' '+m:{A-Z}={a-z}'
# Groups
zstyle ':completion:*:*:*:*:descriptions' format '%F{green}-- %d --%f'
# Group completion
zstyle ':completion:*' group-name ''
# Group type order
zstyle ':completion:*:*:-command-:*:*' group-order alias builtins functions commands
zmodload zsh/complist
# compinit # move to last line
_comp_options+=(globdots) # Include hidden files.

# Vim text-object
autoload -Uz select-bracketed select-quoted
zle -N select-quoted
zle -N select-bracketed
for km in viopp visual; do
  bindkey -M $km -- '-' vi-up-line-or-history
  for c in {a,i}${(s..)^:-\'\"\`\|,./:;=+@}; do
    bindkey -M $km $c select-quoted
  done
  for c in {a,i}${(s..)^:-'()[]{}<>bB'}; do
    bindkey -M $km $c select-bracketed
  done
done

# vim-surround
autoload -Uz surround
zle -N delete-surround surround
zle -N add-surround surround
zle -N change-surround surround
bindkey -M vicmd cs change-surround
bindkey -M vicmd ds delete-surround
bindkey -M vicmd ys add-surround
bindkey -M visual S add-surround

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history

# Emacs keys in vi-insert mode
# FIXME: swap to emacs not work
# select-emacs() { set -o emacs }
# zle -N select-emacs
# bindkey -M viins '[e' select-emacs
bindkey -M viins '^A' beginning-of-line
bindkey -M viins '^E' end-of-line
bindkey -M viins '^R' history-incremental-search-backward
bindkey -M viins '^S' history-incremental-search-forward
bindkey -M viins '^F' forward-char
bindkey -M viins '^B' backward-char
bindkey -M viins '^D' delete-char-or-list
bindkey -M viins '^K' kill-line
bindkey -M viins '^N' down-line-or-history
bindkey -M viins '^P' up-line-or-history

# Home/End
bindkey -M viins '^[[1~' beginning-of-line
bindkey -M viins '^[[4~' end-of-line

# Delete
bindkey -M viins '^[[3~' delete-char-or-list
 
# ctrl+<- | ctrl+->
bindkey "^[[1;5D" backward-word
bindkey "^[[1;5C" forward-word

# Bash like Emacs mode
autoload edit-command-line; zle -N edit-command-line
bindkey '^X^E' edit-command-line
bindkey -M emacs '^X^E' edit-command-line

# TODO :edit hard code shortcut
# Shortcut
bindkey -s '^X^X' "^Ustartx^M"
bindkey -s '^X^S' "^Utmux-ses^M"

# Change cursor shape for different vi modes.
cursor_mode() {
  cursor_block='\e[2 q'
  cursor_beam='\e[6 q'

  function zle-keymap-select {
    if [[ ${KEYMAP} == vicmd ]] ||
        [[ $1 = 'block' ]]; then
        echo -ne $cursor_block
    elif [[ ${KEYMAP} == main ]] ||
        [[ ${KEYMAP} == viins ]] ||
        [[ ${KEYMAP} = '' ]] ||
        [[ $1 = 'beam' ]]; then
        echo -ne $cursor_beam
    fi
  }

  zle-line-init() {
    echo -ne $cursor_beam
  }

  zle -N zle-keymap-select
  zle -N zle-line-init
}

cursor_mode

# Plugin
function zsh_add_file() {
    [ -f "$ZDOTDIR/$1" ] && source "$ZDOTDIR/$1"
}

function zsh_add_plugin() {
    PLUGIN_NAME=$(echo $1 | cut -d "/" -f 2)
    PLUGIN_PATH="$ZDOTDIR/plugins/$PLUGIN_NAME"
    if [ -z "$PLUGIN_PATH" ]; then
        git clone "https://github.com/$1.git" "$ZDOTDIR/plugins/$PLUGIN_NAME"
    fi
    if [ -d "$PLUGIN_PATH" ]; then 
        zsh_add_file "plugins/$PLUGIN_NAME/$PLUGIN_NAME.plugin.zsh" || \
        zsh_add_file "plugins/$PLUGIN_NAME/$PLUGIN_NAME.zsh"
    fi
}

zsh_add_plugin "zsh-users/zsh-autosuggestions"
zsh_add_plugin "zsh-users/zsh-syntax-highlighting"
zsh_add_plugin "zsh-users/zsh-completions"
zsh_add_plugin "hlissner/zsh-autopair"
# zsh_add_plugin "MenkeTechnologies/zsh-expand"
fpath=("$ZDOTDIR/plugins/zsh-completions" $fpath)
compinit

if command -v fzf > /dev/null && [ -f "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf/fzf.zsh ]; then
    source "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf/fzf.zsh
fi

# Modified from Guix auto bash config
if [ -n "$GUIX_ENVIRONMENT" ]; then
    if [[ $PS1 =~ (.*)"\\$" ]]; then
        PS1="${match[1]} [guix env]\\\$ "
    fi
fi

# bun completions
[ -s "/home/pk/.bun/_bun" ] && source "/home/pk/.bun/_bun"
