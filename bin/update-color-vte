#!/usr/bin/env bash
#https://github.com/dylanaraps/pywal/blob/master/pywal/sequences.py

source $MY_COLOR_CACHE_DIR/colors.sh

# if [ -n "$TMUX" ]; then
#   # Tell tmux to pass the escape sequences through
#   # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
#   put_template() { printf '\033Ptmux;\033\033]4;%d;%s\033\033\\\033\\' $@; }
#   put_template_var() { printf '\033Ptmux;\033\033]%d;%s\033\033\\\033\\' $@; }
#   put_template_custom() { printf '\033Ptmux;\033\033]%s%s\033\033\\\033\\' $@; }
# elif [ "${TERM%%[-.]*}" = "screen" ]; then
#   # GNU screen (screen, screen-256color, screen-256color-bce)
#   put_template() { printf '\033P\033]4;%d;rgb:%s\007\033\\' $@; }
#   put_template_var() { printf '\033P\033]%d;rgb:%s\007\033\\' $@; }
#   put_template_custom() { printf '\033P\033]%s%s\007\033\\' $@; }
# elif [ "${TERM%%-*}" = "linux" ]; then
#   put_template() { [ $1 -lt 16 ] && printf "\e]P%x%s" $1 $(echo $2 | sed 's/\///g'); }
#   put_template_var() { true; }
#   put_template_custom() { true; }
# else
put_template() { printf '\033]4;%d;%s\033\\' $@; }
put_template_var() { printf '\033]%d;%s\033\\' $@; }
put_template_custom() { printf '\033]%s%s\033\\' $@; }
# fi

# 16 color space
SEQ="$SEQ$(put_template 0  $color00)"
SEQ="$SEQ$(put_template 1  $color01)"
SEQ="$SEQ$(put_template 2  $color02)"
SEQ="$SEQ$(put_template 3  $color03)"
SEQ="$SEQ$(put_template 4  $color04)"
SEQ="$SEQ$(put_template 5  $color05)"
SEQ="$SEQ$(put_template 6  $color06)"
SEQ="$SEQ$(put_template 7  $color07)"
SEQ="$SEQ$(put_template 8  $color08)"
SEQ="$SEQ$(put_template 9  $color09)"
SEQ="$SEQ$(put_template 10 $color10)"
SEQ="$SEQ$(put_template 11 $color11)"
SEQ="$SEQ$(put_template 12 $color12)"
SEQ="$SEQ$(put_template 13 $color13)"
SEQ="$SEQ$(put_template 14 $color14)"
SEQ="$SEQ$(put_template 15 $color15)"

# 10 = foreground, 11 = background, 12 = cursor foreground
# 13 = mouse foreground, 708 = background border color.
SEQ="$SEQ$(put_template_var 10 $foreground)"
SEQ="$SEQ$(put_template_var 11 $background)"
SEQ="$SEQ$(put_template_var 12 $cursor)"
SEQ="$SEQ$(put_template_var 13 $cursor)"

# # foreground / background / cursor color
# if [ -n "$ITERM_SESSION_ID" ]; then
#   # iTerm2 proprietary escape codes
#   put_template_custom Pg {{base05-hex}} # foreground
#   put_template_custom Ph {{base00-hex}} # background
#   put_template_custom Pi {{base05-hex}} # bold color
#   put_template_custom Pj {{base02-hex}} # selection color
#   put_template_custom Pk {{base05-hex}} # selected text color
#   put_template_custom Pl {{base05-hex}} # cursor
#   put_template_custom Pm {{base00-hex}} # cursor text
# else
#   put_template_var 10 $color_foreground
#   if [ "$BASE16_SHELL_SET_BACKGROUND" != false ]; then
#     put_template_var 11 $color_background
#     if [ "${TERM%%-*}" = "rxvt" ]; then
#       put_template_var 708 $color_background # internal border (rxvt)
#     fi
#   fi
#   put_template_custom 12 ";7" # cursor (reverse video)
# fi

if [ -n $1 ]; then
  if [ -f /dev/pts/$1 ]; then
    echo $SEQ > /dev/pts/$1
  else
    echo "no file /dev/pts/$1"
  fi
else
  for t in /dev/pts/[0-9]
  do
    echo $t
    echo $SEQ > $t
  done
fi
