local wezterm = require("wezterm")
local action = wezterm.action

config = {
	color_scheme = "Gruvbox light, soft (base16)",
	use_fancy_tab_bar = false,
	hide_tab_bar_if_only_one_tab = true,
	font_size = 11,
	disable_default_key_bindings = true,
}

local default_mods = "CTRL|SHIFT|ALT"
config.keys = {
	{ key = "L", mods = default_mods, action = action.ShowDebugOverlay },
}

return config
