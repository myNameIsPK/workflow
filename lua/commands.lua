vim.api.nvim_create_user_command("SudoW", "w !sudo tee %", { desc = "Write file with sudo permission" })
vim.api.nvim_create_user_command("RemoveThisFile", function()
  local current_file = vim.fn.getreg "%"
  local choice = vim.fn.confirm("Are you sure to delete " .. current_file, "&Yes\n&No")
  if choice == 1 then
    print("remove " .. current_file)
    vim.fn.delete(current_file)
  end
end, { desc = "Remove the current file" })

vim.api.nvim_create_user_command("QfToggle", function()
  local function qf_exist()
    for _, w in ipairs(vim.fn.getwininfo()) do
      if w.quickfix == 1 then
        return true
      end
    end
    return false
  end

  if qf_exist() then
    vim.notify "cclose"
    vim.cmd.cclose()
  else
    vim.notify "copen"
    vim.cmd.copen()
  end
end, { desc = "Quixfix window toggle" })
