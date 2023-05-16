#!/usr/bin/env sh

xrdb_file="${XDG_CONFIG_HOME:-$HOME/.config}/x11/xresources"

reload_xrdb ()
{
  xrdb -merge $1
}

reload_bspwm ()
{
  bspc wm -r
}

reload_polybar ()
{
  pkill -USR1 -x polybar
}

# FIXME: not work
reload_kitty ()
{
  kitty @ set-colors --all --configured ${MY_COLOR_CACHE_DIR}/colors_kitty.conf
}

reload_tmux ()
{
  [ -e $TMUX ] && tmux source $XDG_CONFIG_HOME/tmux/tmux.con
}

reload_vte ()
{
  update-color-vte.bash
}

reload_all ()
{
  reload_xrdb $xrdb_file &
  reload_polybar &
  reload_bspwm &
  reload_kitty &
  reload_tmux &
  reload_vte &
}

reload_all 2>&1 | tee -a "/tmp/colors_reload.log"
