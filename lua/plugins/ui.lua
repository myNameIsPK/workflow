return {
  {
    "folke/which-key.nvim",
    event = "VimEnter",
    config = function()
      require("which-key").setup {
        window = {
          margin = { 0, 0, 0, 0 }, -- extra window margin [top, right, bottom, left]
          padding = { 0, 0, 0, 0 }, -- extra window padding [top, right, bottom, left]
        },
      }
      require("which-key").register {
        ["<leader>f"] = "Telescope",
        ["<leader>d"] = "Diagnostics",
        ["<leader>h"] = "Helps",
        ["<leader>s"] = "Status",
        ["<leader>t"] = "Toggle",
        ["<leader>n"] = "Notes",
      }
    end,
  },

  { "kyazdani42/nvim-web-devicons" },

  {
    "j-hui/fidget.nvim",
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
