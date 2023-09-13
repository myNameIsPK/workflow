local M = {}

function M.quickfixtext(info)
  local items = vim.fn.getqflist()
  local l = {}
  for idx = info.start_idx, info.end_idx do
    local entry = ""
    local filename = ""
    if items[idx].bufnr and items[idx].lnum ~= 0 then
      filename = vim.fn.bufname(items[idx].bufnr)
      entry = entry .. filename
    end
    entry = entry .. "|"

    if items[idx].lnum ~= 0 then
      entry = entry .. items[idx].lnum .. ":"
    end
    if items[idx].col ~= 0 then
      entry = entry .. items[idx].col
    end
    entry = entry .. "|"

    if items[idx].text then
      entry = entry .. items[idx].text
    end
    table.insert(l, entry)
  end
  return l
end

return M
