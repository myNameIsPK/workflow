-- vim: fmr={{{,}}} fdm=marker:
-- stylua: ignore start
-- Init {{{
local p = require("themes.palette")
local c = {}

c.neutral_red = p.neutral_red
c.neutral_green = p.neutral_green
c.neutral_yellow = p.neutral_yellow
c.neutral_blue = p.neutral_blue
c.neutral_purple = p.neutral_purple
c.neutral_aqua = p.neutral_aqua
c.neutral_orange = p.neutral_orange

local is_dark = (vim.opt.background:get() == "dark")

if is_dark then
  c.bg0 = p.dark0
  c.bg1 = p.dark1
  c.bg2 = p.dark2
  c.bg3 = p.dark3
  c.bg4 = p.dark4

  c.fg0 = p.light0
  c.fg1 = p.light1
  c.fg2 = p.light2
  c.fg3 = p.light3
  c.fg4 = p.light4

  c.red = p.bright_red
  c.green = p.bright_green
  c.yellow = p.bright_yellow
  c.blue = p.bright_blue
  c.purple = p.bright_purple
  c.aqua = p.bright_aqua
  c.orange = p.bright_orange
else
  c.bg0 = p.light0
  c.bg1 = p.light1
  c.bg2 = p.light2
  c.bg3 = p.light3
  c.bg4 = p.light4

  c.fg0 = p.dark0
  c.fg1 = p.dark1
  c.fg2 = p.dark2
  c.fg3 = p.dark3
  c.fg4 = p.dark4

  c.red = p.faded_red
  c.green = p.faded_green
  c.yellow = p.faded_yellow
  c.blue = p.faded_blue
  c.purple = p.faded_purple
  c.aqua = p.faded_aqua
  c.orange = p.faded_orange
end

if vim.fn.has("nvim") == 1 then
  vim.g.terminal_color_0 = c.bg0[1]
  vim.g.terminal_color_1 = c.neutral_red[1]
  vim.g.terminal_color_2 = c.neutral_green[1]
  vim.g.terminal_color_3 = c.neutral_yellow[1]
  vim.g.terminal_color_4 = c.neutral_blue[1]
  vim.g.terminal_color_5 = c.neutral_purple[1]
  vim.g.terminal_color_6 = c.neutral_aqua[1]
  vim.g.terminal_color_7 = c.fg4[1]
  vim.g.terminal_color_8 = c.bg4[1]
  vim.g.terminal_color_9 = c.red[1]
  vim.g.terminal_color_10 = c.green[1]
  vim.g.terminal_color_11 = c.yellow[1]
  vim.g.terminal_color_12 = c.blue[1]
  vim.g.terminal_color_13 = c.purple[1]
  vim.g.terminal_color_14 = c.aqua[1]
  vim.g.terminal_color_15 = c.fg0[1]
end

c = vim.tbl_map(function(v) return v[1] end, c)

-- }}}

-- TODO: colorsheme setting

-- stylua: ignore end
