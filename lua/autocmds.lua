vim.cmd(
  [[
    augroup spell
      au!
      au BufWritePost *.utf-8.add :mkspell! ~/.config/nvim/spell/en.utf-8.add
    augroup END
  ]]
)
