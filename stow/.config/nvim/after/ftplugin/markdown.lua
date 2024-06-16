-- vim.opt_local.spell = true
vim.opt_local.wrap = true
vim.opt_local.linebreak = true
vim.opt_local.breakindent = true
vim.opt_local.conceallevel = 2
vim.opt_local.concealcursor = "nc"

local function mdlink_from_clipboard()
  local pos = vim.api.nvim_win_get_cursor(0)
  local clipboard = vim.fn.getreg "+"
  vim.api.nvim_buf_set_text(0, pos[1] - 1, pos[2], pos[1] - 1, pos[2], { "[](" .. clipboard .. ")" })
  vim.api.nvim_win_set_cursor(0, { pos[1], pos[2] + 1 })
  vim.cmd.startinsert()
end

local function md_toggle_task_list()
  local repl, count = vim.api.nvim_get_current_line():gsub("%[ %]", "[x]")
  if count > 0 then
    vim.api.nvim_set_current_line(repl)
    return
  else
    local new_repl = vim.api.nvim_get_current_line():gsub("%[x%]", "[ ]")
    vim.api.nvim_set_current_line(new_repl)
  end
end

vim.keymap.set("n", "<localleader>p", mdlink_from_clipboard, { buffer = true, desc = "Paste Markdown Link" })
vim.keymap.set("n", "<localleader>t", md_toggle_task_list, { buffer = true, desc = "Toggle Task list" })
