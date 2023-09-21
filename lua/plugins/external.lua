return {
  {
    "iamcco/markdown-preview.nvim",
    cmd = "MarkdownPreviewToggle",
    init = function()
      vim.keymap.set("n", "<leader>tmp", "<Cmd>MarkdownPreviewToggle<Cr>", { desc = "Markdown Preview" })
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
    cmd = "Slime",
    init = function()
      vim.g.slime_no_mappings = 1
    end,
    keys = {
      { "<leader>cs", "<Plug>SlimeParagraphSend" },
      { "<leader>cs", "<Plug>SlimeRegionSend" , mode = "v"},
      { "<leader>cR", "<Plug>SlimeConfig" },
      { "<leader>c.", "<Plug>SlimeLineSend" },
      { "<leader>C", "<Plug>SlimeMotionSend" },
      { "<leader>cc", "<Plug>SlimeSendCell" },
    };
    config = function()
      vim.g.slime_target = "tmux"
      vim.g.slime_bracketed_paste = 1
      vim.g.slime_default_config = {
        socket_name = "default",
        target_pane = "{marked}",
      }
      vim.g.slime_cell_delimiter = "# %%"
    end,
  },
}
