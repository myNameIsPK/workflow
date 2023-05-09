#!/usr/bin/env sh

xrdb_file="${XDG_CONFIG_HOME:-$HOME/.config}/x11/xresources"
reload_xrdb ()
{
  xrdb -merge $1
}

reload_polybar ()
{
  pkill -USR1 -x polybar
}

reload_all ()
{
  reload_xrdb $xrdb_file
  reload_polybar
}

reload_all
