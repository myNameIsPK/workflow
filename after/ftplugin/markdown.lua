-- vim.opt_local.spell = true
vim.opt_local.wrap = true
vim.opt_local.linebreak = true
vim.opt_local.breakindent = true
vim.opt_local.conceallevel = 1

local function mdlink_from_clipboard()
  local pos = vim.api.nvim_win_get_cursor(0)
  local clipboard = vim.fn.getreg "+"
  vim.api.nvim_buf_set_text(0, pos[1] - 1, pos[2], pos[1] - 1, pos[2], { "[](" .. clipboard .. ")" })
  vim.api.nvim_win_set_cursor(0, { pos[1], pos[2] + 1 })
  vim.cmd.startinsert()
end

vim.keymap.set("n", "<leader>p", mdlink_from_clipboard, { buffer = true, desc = "Paste Markdown Link" })
