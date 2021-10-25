vim.cmd(
  [[
    augroup spell
      au!
      au BufWritePost *.utf-8.add :mkspell! ~/.config/nvim/spell/en.utf-8.add
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
