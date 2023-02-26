-- TODO: this is content of todo comment
-- regular comment
-- TEST: anothor tag
local text = "This is code"
local function append(t)
  t = t .. " and TODO in string of code"
  return t
end
append(text) -- TODO: this is todo too

--[[ TODO: in multi line comment
content ]]

-- (comment content: "comment_content" @todo_content (#match? @todo_content "TODO")) @todo_comment
local todo_query = vim.treesitter.query.parse_query(
  "lua",
  '(comment content: "comment_content" @todo_content (#match? @todo_content "TODO")) @todo_comment'
)

local get_root = function(bufnr)
  local parser = vim.treesitter.get_parser(bufnr, "lua")
  local tree = parser:parse()[1]
  return tree:root()
end

local get_todo = function(bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  local root = get_root(bufnr)
  if vim.bo[bufnr].filetype ~= "lua" then
    error "usable only lua filetype"
  end
  local todo_lines = {}
  for id, node, metadata in todo_query:iter_captures(root, bufnr, 0, -1) do
    if todo_query.captures[id] == "todo_comment" then
      local range = { node:range() }
      table.insert(todo_lines, { range, vim.api.nvim_buf_get_lines(bufnr, range[1], range[3] + 1, false) })
    end
  end
  return todo_lines
end

I(get_todo())
