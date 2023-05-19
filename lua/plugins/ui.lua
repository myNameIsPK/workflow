return {
  { "folke/which-key.nvim" },

  { "kyazdani42/nvim-web-devicons" },

  {
    "j-hui/fidget.nvim",
    lazy = false,
    config = true,
  },

  { "norcalli/nvim-colorizer.lua", cmd = "ColorizerToggle" },

  -- { "jbyuki/nabla.nvim" },

  {
    "sainnhe/gruvbox-material",
    lazy = false,
    priority = 1000,
    config = function()
      vim.g.gruvbox_material_visual = "reverse"
      vim.g.gruvbox_material_background = "soft"
      vim.g.gruvbox_material_disable_italic_comment = 0
      vim.g.gruvbox_material_palette = "original"
      vim.cmd("colorscheme gruvbox-material")
    end,
  },
}
