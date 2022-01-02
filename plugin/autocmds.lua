vim.cmd(
  [[
    augroup TextYankHighlight
      au!
      au TextYankPost * silent! lua vim.highlight.on_yank()
    augroup END
  ]]
)

vim.cmd(
  [[
    augroup SpellAdd
      au!
      au BufWritePost *.utf-8.add :mkspell! ~/.config/nvim/spell/en.utf-8.add
    augroup END
  ]]
)

vim.cmd "au FileType man nnoremap <buffer><silent> q :quit<CR>"

-- FIXME: file not source
-- vim.cmd(
--   [[
--     augroup aliases
--       au!
--       au BufWritePost $XDG_CONFIG_HOME/shell/aliases !source $XDG_CONFIG_HOME/shell/aliases
--     augroup END
--   ]]
-- )

vim.cmd(
  [[
    augroup Xresources
      au!
      au BufWritePost xresources :!xrdb -merge ~/.config/x11/xresources
    augroup END
  ]]
)
