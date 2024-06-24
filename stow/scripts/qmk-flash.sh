#!/usr/bin/env bash

set -euo pipefail

KEYBOARD="${KEYBOARD:-xiudi/xd75}"
KEYMAP="${KEYMAP:-my_maps}"

firmware_path="${MY_SRC:-$HOME/.local/src}/qmk_firmware"
config_path="${HOME}/Projects/qmk-keymaps"

keymap_path="${config_path}/${KEYBOARD}/${KEYMAP}"
dest_path="${firmware_path}/keyboards/${KEYBOARD}/keymaps/${KEYMAP}"

if [[ ! -d $firmware_path ]]; then
  echo "Clone qmk_firmware from github"
  mkdir -p ${firmware_path%%qmk_firmware}
  cd ${firmware_path%%qmk_firmware}
  git clone "https://github.com/qmk/qmk_firmware.git"
fi

# refresh repeat rate (same as in .xprofile)
refresh ()
{
  if command -v xset > /dev/null ; then
    sleep 1
    echo "Refresh repeat rate"
    exec xset r rate 300 50 &
  else
    echo "No xset So no Refresh"
  fi
}

clear ()
{
  echo "Remove keymap in ${dest_path}"
  rm -rf ${dest_path}
}

trap 'clear' ERR

echo "Copy keymap to ${dest_path}"
cp -rf $keymap_path "${dest_path}"

cd $firmware_path
# util/docker_build.sh $KEYBOARD:$KEYMAP:flash
make $KEYBOARD:$KEYMAP:flash && refresh

clear
