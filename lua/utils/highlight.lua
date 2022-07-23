local M = {}

function M.highlight(group, colors, child)
  -- setup funtion
  colors.sp = colors.sp or "none"
  colors.style = colors.style or "none"
  colors.fg = colors.fg or { "none", "none" }
  colors.bg = colors.bg or { "none", "none" }
  local g_foreground = colors.fg[1]
  local c_foreground = colors.fg[2]
  local g_background = colors.bg[1]
  local c_background = colors.bg[2]
  local guisp = colors.sp[1] or "none"
  local style = colors.style or "none"
  -- stylua: ignore
  vim.cmd(string.format(
  'hi %s guifg=%s guibg=%s guisp=%s gui=%s ctermfg=%s ctermbg=%s cterm=%s',
  group, g_foreground, g_background, guisp, style, c_foreground, c_background, style
  ))

  if child ~= nil and type(child) == "table" then
    for _, v in ipairs(child) do
      M.hi_link(v, group)
    end
  end
end

function M.hi_link(group, to, child)
  vim.cmd("hi! link " .. group .. " " .. to)

  if child ~= nil and type(child) == "table" then
    for _, v in ipairs(child) do
      M.hi_link(v, group)
    end
  end
end

return M
