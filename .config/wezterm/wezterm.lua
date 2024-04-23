local wezterm = require("wezterm")
local action = wezterm.action

config = {}
config.color_scheme = "Gruvbox light, soft (base16)"
config.use_fancy_tab_bar = false
config.hide_tab_bar_if_only_one_tab = true
config.font_size = 11
config.disable_default_key_bindings = true
config.window_padding = {
	left = 2,
	right = 2,
	top = 10,
	bottom = 0,
}
config.cursor_blink_rate = 500
config.cursor_blink_ease_in = "Constant"
config.cursor_blink_ease_out = "Constant"
-- disable ligature
config.harfbuzz_features = { "calt=0", "clig=0", "liga=0" }

local default_mods = "CTRL|SHIFT|ALT"
config.keys = {
	{ key = "L", mods = default_mods, action = action.ShowDebugOverlay },
	{ key = "X", mods = default_mods, action = action.ActivateCopyMode },
}

return config
