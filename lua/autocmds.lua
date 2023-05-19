local function group(group_name)
  return vim.api.nvim_create_augroup(group_name, { clear = true })
end
local autocmd = vim.api.nvim_create_autocmd

-- -- This regular auto group
-- vim.cmd([[
--   augroup _general_settings
--     autocmd!
--     autocmd TextYankPost * silent! lua vim.highlight.on_yank()
--   augroup END
-- ]])
autocmd("TextYankPost", {
  group = group "_general_settings",
  -- pattern = "*",
  callback = function()
    vim.highlight.on_yank()
  end,
  -- command = "silent! lua vim.highlight.on_yank()", -- or use command instead of callback
  desc = "Highlight yanked text",
})

autocmd("TermOpen", {
  group = group "_terminal_open",
  pattern = "term://*",
  callback = function()
    vim.opt_local.number = false
    vim.opt_local.relativenumber = false
    vim.cmd.startinsert()
  end,
  desc = "Fix terminal behavior",
})

autocmd("TermClose", {
  group = group "_terminal_close",
  pattern = "term://*",
  callback = function()
    vim.fn.feedkeys "q"
  end,
  desc = "Close without showing exitcode",
})

vim.cmd [[

    " augroup _smart_relativenumber
    "   autocmd!
    "   autocmd InsertEnter * :set norelativenumber
    "   autocmd InsertLeave * :set relativenumber
    " augroup END

    augroup _iminsert
      autocmd!
      autocmd InsertLeave * :set iminsert=0
    augroup END

    augroup _quit_with_q
      autocmd!
      autocmd FileType qf,help,man,lspinfo nnoremap <silent> <buffer> q :close<CR>
    augroup END

    augroup _reload_config
      autocmd BufWritePost *.utf-8.add :mkspell! ~/.config/nvim/spell/en.utf-8.add
      autocmd BufWritePost xresources :silent !xrdb -merge ~/.config/x11/xresources
      autocmd BufWritePost sxhkdrc :silent !pkill -USR1 -x sxhkd; notify-send sxhkd reloaded\!\!
      autocmd BufWritePost skippy-xd.rc :silent !pkill -USR1 -x skippy-xd; notify-send skippy-xd reloaded\!\!
      autocmd BufWritePost $XDG_CONFIG_HOME/polybar/config.ini :silent !pkill -USR1 -x polybar && notify-send polybar reloaded\!\!
      autocmd BufWritePost $XDG_CONFIG_HOME/colorsync/config.json :silent !colorsync && template-gen && reload
    augroup END

    augroup _compile
      autocmd BufWritePost *.tex :silent !latexmk -pdf -interaction=nonstopmode %
    augroup END

    augroup _ansible
      autocmd!
      autocmd BufRead *.yaml,*.yml if search('hosts:\|tasks:', 'nw') | set ft=yaml.ansible | endif
    augroup END

  ]]
