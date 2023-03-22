if vim.g.loaded_my_todo_comment == 1 then
  return
end
vim.g.loaded_my_todo_comment = 1

local list_todos = require("todo-comment").list_todo_comments
vim.api.nvim_create_user_command("TodoComments", function()
  list_todos()
end, {})
