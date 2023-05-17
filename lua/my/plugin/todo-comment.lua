local M = {}

-- TODO: use todo keywords and hilight group
local todo_keywords = {
  FIX = {
    icon = " ", -- icon used for the sign, and in search results
    color = "error", -- can be a hex color, or a named color (see below)
    alt = { "FIXME", "BUG", "FIXIT", "ISSUE" }, -- a set of other keywords that all map to this FIX keywords
    -- signs = false, -- configure signs for some keywords individually
  },
  TODO = { icon = " ", color = "info" },
  HACK = { icon = " ", color = "warning" },
  WARN = { icon = " ", color = "warning", alt = { "WARNING", "XXX" } },
  PERF = { icon = " ", alt = { "OPTIM", "PERFORMANCE", "OPTIMIZE" } },
  NOTE = { icon = " ", color = "hint", alt = { "INFO" } },
}

M.vimgrep_cmd = { "rg", "--vimgrep" }

function M.get_keyword_list(keywords)
  local keywords_list = {}
  for k, v in pairs(keywords) do
    if type(k) == "string" then
      table.insert(keywords_list, k)
    end
    if v.alt then
      for _, word in ipairs(v.alt) do
        table.insert(keywords_list, #keywords_list + 1, word)
      end
    end
  end
  return keywords_list
end

function M.gen_todo_pattern(keywords)
  local pattern = '"('
  for _, word in ipairs(keywords) do
    pattern = pattern .. word .. "|"
  end
  pattern = pattern.gsub(pattern, "|$", ')\\(?.*\\)?:"')
  return pattern
end

-- FIXME: delete EOF line ("") from data
local function jobcallback(_, data, event)
  if event == "stdout" then
    vim.fn.setqflist({}, "a", {
      title = "TODO COMMENTS",
      lines = data,
      efm = "%f:%l:%c:%m",
    })
    vim.cmd.copen()
  elseif event == "stderr" then
    vim.notify("Todo comments Error" .. I(data), vim.log.levels.ERROR)
  end
end

function M.list_todo_comments()
  table.insert(M.vimgrep_cmd, "TODO")
  vim.fn.jobstart(M.vimgrep_cmd, {
    stdin = "null",
    stdout_buffered = true,
    on_exit = jobcallback,
    on_stdout = jobcallback,
    on_stderr = jobcallback,
  })
end

return M
