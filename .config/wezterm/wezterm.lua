local wezterm = require("wezterm")
local act = wezterm.action
local config = {}

-- config.color_scheme = "Gruvbox light, soft (base16)"
config.use_fancy_tab_bar = false
config.hide_tab_bar_if_only_one_tab = true
config.term = "wezterm"
config.font_size = 11
config.font = wezterm.font_with_fallback({
	"JetBrains Mono",
	"Noto Color Emoji",
	"WenQuanYi Zen Hei",
})
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
local csa = "CTRL|SHIFT|ALT"
local ca = "CTRL|ALT"
config.keys = {
	{ key = "c", mods = csa, action = act.CopyTo("Clipboard") },
	{ key = "v", mods = csa, action = act.PasteFrom("Clipboard") },
	{ key = "t", mods = csa, action = act.SpawnTab("CurrentPaneDomain") },
	{ key = "q", mods = csa, action = act.CloseCurrentTab({ confirm = true }) },
	{ key = "n", mods = csa, action = act.SpawnWindow },
	{ key = '"', mods = csa, action = act.SplitVertical({ domain = "CurrentPaneDomain" }) },
	{ key = "%", mods = csa, action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
	{ key = "x", mods = csa, action = act.ActivateCommandPalette },
	{ key = "l", mods = csa, action = act.ShowDebugOverlay },
	{ key = "+", mods = csa, action = act.IncreaseFontSize },
	{ key = "_", mods = csa, action = act.DecreaseFontSize },
	{ key = "f", mods = csa, action = act.Search({ CaseSensitiveString = "" }) },
	{ key = "Tab", mods = csa, action = act.ActivateTabRelative(1) },
	{ key = "Tab", mods = ca, action = act.ActivateTabRelative(-1) },
	{ key = "Escape", mods = csa, action = act.ActivateCopyMode },
	{ key = "Space", mods = csa, action = act.QuickSelect },
	{ key = "LeftArrow", mods = csa, action = act.ActivatePaneDirection("Left") },
	{ key = "LeftArrow", mods = ca, action = act.AdjustPaneSize({ "Left", 1 }) },
	{ key = "RightArrow", mods = csa, action = act.ActivatePaneDirection("Right") },
	{ key = "RightArrow", mods = ca, action = act.AdjustPaneSize({ "Right", 1 }) },
	{ key = "UpArrow", mods = csa, action = act.ActivatePaneDirection("Up") },
	{ key = "UpArrow", mods = ca, action = act.AdjustPaneSize({ "Up", 1 }) },
	{ key = "DownArrow", mods = csa, action = act.ActivatePaneDirection("Down") },
	{ key = "DownArrow", mods = ca, action = act.AdjustPaneSize({ "Down", 1 }) },
}

package.path = package.path .. ";" .. os.getenv("HOME") .. "/.cache/colorsync/?.lua"
config.colors = require("colors_wezterm")

return config
