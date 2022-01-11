vim.cmd(
  [[
    augroup TextYankHighlight
      au!
      au TextYankPost * silent! lua vim.highlight.on_yank()
    augroup END

    augroup SpellAdd
      au!
      au BufWritePost *.utf-8.add :mkspell! ~/.config/nvim/spell/en.utf-8.add
    augroup END

    augroup Xresources
      au!
      au BufWritePost xresources :!xrdb -merge ~/.config/x11/xresources
    augroup END

    augroup Sxhkdrc
      au!
      au BufWritePost sxhkdrc :!pkill -USR1 -x sxhkd; notify-send reload sxhkd
    augroup END
  ]]
)

-- FIXME: file not source
-- vim.cmd(
--   [[
--     augroup aliases
--       au!
--       au BufWritePost $XDG_CONFIG_HOME/shell/aliases !source $XDG_CONFIG_HOME/shell/aliases
--     augroup END
--   ]]
-- )
