vim.api.nvim_create_user_command("SudoW", "w !sudo tee %", { desc = "Write file with sudo permission" })
vim.api.nvim_create_user_command("RemoveThisFile", function()
  local current_file = vim.fn.getreg "%"
  local choice = vim.fn.confirm("Are you sure to delete " .. current_file, "&Yes\n&No")
  if choice == 1 then
    print("remove " .. current_file)
    vim.fn.delete(current_file)
  end
end, { desc = "Remove the current file" })
