local wezterm = require("wezterm")
local action = wezterm.action
local config = {}

-- config.color_scheme = "Gruvbox light, soft (base16)"
config.use_fancy_tab_bar = false
config.hide_tab_bar_if_only_one_tab = true
config.font_size = 11
-- config.enable_scroll_bar = true
config.window_padding = {
	left = 2,
	right = 2, -- also width of scroll bar
	top = 0,
	bottom = 0,
}
config.cursor_blink_rate = 500
config.cursor_blink_ease_in = "Constant"
config.cursor_blink_ease_out = "Constant"
-- disable ligature
config.harfbuzz_features = { "calt=0", "clig=0", "liga=0" }

config.disable_default_key_bindings = true
local default_mods = "CTRL|SHIFT|ALT"
config.keys = {
	{ key = "l", mods = default_mods, action = action.ShowDebugOverlay },
	{ key = "Escape", mods = default_mods, action = action.ActivateCopyMode },
}

package.path = package.path .. ";" .. os.getenv("HOME") .. "/.cache/colorsync/?.lua"
config.colors = require("colors_wezterm")

return config
