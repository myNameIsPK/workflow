return {
  {
    "folke/which-key.nvim",
    -- event = "VimEnter",
    config = function()
      require("which-key").setup {
        window = {
          margin = { 0, 0, 0, 0 }, -- extra window margin [top, right, bottom, left]
          padding = { 0, 0, 0, 0 },
        },
      }
    end,
  },

  { "kyazdani42/nvim-web-devicons" },

  {
    "j-hui/fidget.nvim",
    -- FIXME: fidget cause the crash when `lua-ls@3.6.20` load some shit
    -- cond = false,
    tag = "legacy",
    lazy = false,
    config = true,
  },

  {
    "norcalli/nvim-colorizer.lua",
    cmd = "ColorizerToggle",
    keys = { { "<leader>tc", "<Cmd>ColorizerToggle<Cr>" } },
  },

  -- { "jbyuki/nabla.nvim" },

  {
    "sainnhe/gruvbox-material",
    lazy = false,
    priority = 1000,
    cond = my.opts.colorscheme.use_plugin,
    config = function()
      vim.g.gruvbox_material_visual = "reverse"
      vim.g.gruvbox_material_background = "soft"
      vim.g.gruvbox_material_disable_italic_comment = 0
      vim.g.gruvbox_material_palette = "original"
      vim.cmd.colorscheme "gruvbox-material"
      vim.api.nvim_set_hl(0, "DiagnosticUnnecessary", { link = "Comment" })
    end,
  },
}
