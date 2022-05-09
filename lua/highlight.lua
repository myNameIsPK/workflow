local plugin_installed = require("utils.plugins").plugin_installed

-- Set Colorscheme
if plugin_installed("gruvbox-material") then
  -- Colors:Gruvbox-material
  vim.g.gruvbox_material_visual = "reverse"
  vim.g.gruvbox_material_background = "soft"
  vim.g.gruvbox_material_disable_italic_comment = 0
  vim.g.gruvbox_material_palette = "original"
  vim.cmd "colorscheme gruvbox-material"
else
  -- Colors:Gruvbox
  vim.g.gruvbox_contrast_dark = "soft"
  vim.g.gruvbox_italic = 1
  vim.cmd "colorscheme gruvbox"
end
