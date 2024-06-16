let $plenary_path = stdpath("data") .. "/lazy/plenary.nvim"

set rtp+=.
set rtp+=$plenary_path

runtime! plugin/plenary.vim
