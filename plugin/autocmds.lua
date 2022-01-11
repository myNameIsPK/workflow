vim.cmd(
  [[
    augroup _general_settings
      autocmd!
      autocmd TextYankPost * silent! lua vim.highlight.on_yank()
      autocmd FileType qf,help,man,lspinfo nnoremap <silent> <buffer> q :close<CR>
    augroup END

    augroup SpellAdd
      autocmd!
      autocmd BufWritePost *.utf-8.add :mkspell! ~/.config/nvim/spell/en.utf-8.add
    augroup END

    augroup Xresources
      autocmd!
      autocmd BufWritePost xresources :!xrdb -merge ~/.config/x11/xresources
    augroup END

    augroup Sxhkdrc
      autocmd!
      autocmd BufWritePost sxhkdrc :!pkill -USR1 -x sxhkd; notify-send reload sxhkd
    augroup END
  ]]
)

-- FIXME: file not source
-- vim.cmd(
--   [[
--     augroup aliases
--       autocmd!
--       autocmd BufWritePost $XDG_CONFIG_HOME/shell/aliases !source $XDG_CONFIG_HOME/shell/aliases
--     augroup END
--   ]]
-- )
