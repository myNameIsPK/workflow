return {
  {
    "iamcco/markdown-preview.nvim",
    cmd = "MarkdownPreviewToggle",
    init = function()
      vim.keymap.set("n", "<leader>tm", "<Cmd>MarkdownPreviewToggle<Cr>", { desc = "Markdown Preview" })
    end,
    build = function()
      vim.fn["mkdp#util#install"]()
    end,
  },

  {
    "nvim-telescope/telescope-fzf-native.nvim",
    dependencies = "nvim-telescope/telescope.nvim",
    build = "make",
  },

  {
    "jpalardy/vim-slime",
    lazy = false,
    init = function()
      vim.g.slime_no_mappings = 1
    end,
    config = function()
      vim.g.slime_target = "tmux"
      vim.g.slime_bracketed_paste = 1
      vim.g.slime_default_config = {
        socket_name = "default",
        target_pane = "{marked}",
      }
      vim.g.slime_cell_delimiter = "#%%"
      vim.keymap.set("n" , "<leader>cc", "<Plug>SlimeParagraphSend")
      vim.keymap.set("v" , "<leader>cc", "<Plug>SlimeRegionSend")
      vim.keymap.set("n" , "<leader>cv", "<Plug>SlimeConfig")
      vim.keymap.set("n" , "<leader>cl", "<Plug>SlimeLineSend")
      vim.keymap.set("n" , "<leader>C", "<Plug>SlimeMotionSend")
      vim.keymap.set("n" , "<leader>cL", "<Plug>SlimeSendCell")
    end,
  },
}
