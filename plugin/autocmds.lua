local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd

-- -- This regular auto group
-- vim.cmd([[
--   augroup _general_settings
--     autocmd!
--     autocmd TextYankPost * silent! lua vim.highlight.on_yank()
--   augroup END
-- ]])
local group = augroup("_general_settings", { clear = true })
autocmd({ "TextYankPost" }, {
  group = group,
  -- pattern = "*",
  callback = function() vim.highlight.on_yank() end,
  -- command = "silent! lua vim.highlight.on_yank()", -- or use command instead of callback
  desc = "Highlight yanked text"
})

vim.cmd(
  [[

    " augroup _smart_relativenumber
    "   autocmd!
    "   autocmd InsertEnter * :set norelativenumber
    "   autocmd InsertLeave * :set relativenumber
    " augroup END

    augroup _quit_with_q
      autocmd!
      autocmd FileType qf,help,man,lspinfo nnoremap <silent> <buffer> q :close<CR>
    augroup END

    augroup _spell_add
      autocmd!
      autocmd BufWritePost *.utf-8.add :mkspell! ~/.config/nvim/spell/en.utf-8.add
    augroup END

    augroup _xresources
      autocmd!
      autocmd BufWritePost xresources :!xrdb -merge ~/.config/x11/xresources
    augroup END

    augroup _sxhkdrc
      autocmd!
      autocmd BufWritePost sxhkdrc :!pkill -USR1 -x sxhkd; notify-send sxhkd reloaded\!\!
    augroup END

    augroup _pdf_latex
      autocmd!
      autocmd BufWritePost *.tex :!latexmk -pdf -interaction=nonstopmode %
    augroup END

    augroup _polybar
      autocmd!
      autocmd BufWritePost $XDG_CONFIG_HOME/polybar/config :!polybar -rq mybar && notify-send polybar reloaded\!\!
    augroup END

    augroup _ansible
      autocmd!
      autocmd BufRead *.yaml,*.yml if search('hosts:\|tasks:', 'nw') | set ft=yaml.ansible | endif
    augroup END

  ]]
)
