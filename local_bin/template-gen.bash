#!/usr/bin/env bash

set -euo pipefail

COLOR_CACHE=${XDG_CACHE_HOME}
COLOR_TEMPLATES=${XDG_CONFIG_HOME}/colorsync/template/

template_suffix="template"
err_log="/tmp/color_template.log"

if ! test -d ${COLOR_TEMPLATES}; then
  mkdir ${COLOR_TEMPLATES}
fi

source ${COLOR_CACHE}/colorsync/colors.sh

for file_path in ${COLOR_TEMPLATES}*.${template_suffix}
do
  eval "echo -E \"$(cat ${file_path})\"" > $COLOR_CACHE/colorsync/$(basename -s ".$template_suffix" $file_path) 2>> $err_log
done
