#!/usr/bin/env bash
export STEAM_COMPAT_DATA_PATH=~/.proton
export STEAM_COMPAT_CLIENT_INSTALL_PATH=~/.steam/steam
PROTON=~/.steam/steam/steamapps/common/Proton\ 7.0/proton
# PROTON=~/.steam/steam/compatibilitytools.d/GE-Proton7-47/proton
EXE=$@
cd $(dirname "$EXE")
"$PROTON" run "$EXE"
