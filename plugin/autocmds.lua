vim.cmd(
  [[
    augroup _general_settings
      autocmd!
      autocmd TextYankPost * silent! lua vim.highlight.on_yank()
      autocmd FileType qf,help,man,lspinfo nnoremap <silent> <buffer> q :close<CR>
    augroup END

    " augroup _smart_relativenumber
    "   autocmd!
    "   autocmd InsertEnter * :set norelativenumber 
    "   autocmd InsertLeave * :set relativenumber 
    " augroup END

    augroup spellAdd
      autocmd!
      autocmd BufWritePost *.utf-8.add :mkspell! ~/.config/nvim/spell/en.utf-8.add
    augroup END

    augroup xresources
      autocmd!
      autocmd BufWritePost xresources :!xrdb -merge ~/.config/x11/xresources
    augroup END

    augroup sxhkdrc
      autocmd!
      autocmd BufWritePost sxhkdrc :!pkill -USR1 -x sxhkd; notify-send sxhkd reloaded\!\!
    augroup END

    augroup pdf_latex
      autocmd!
      autocmd BufWritePost *.tex :!latexmk -pdf -interaction=nonstopmode %
    augroup END

    augroup _polybar
      autocmd!
      autocmd BufWritePost $XDG_CONFIG_HOME/polybar/config :!polybar -rq mybar && notify-send polybar reloaded\!\!
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
