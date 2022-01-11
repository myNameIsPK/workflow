vim.cmd [[
  augroup ManKeymap
    au!
    au FileType man nnoremap <buffer><silent> q :quit<CR>"
  augroup End
]]
