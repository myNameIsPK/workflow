local M = {}

function M.setup()

  local ok, autopairs = pcall(require, "nvim-autopairs")
  if not ok then
    return
  end

  -- from LunarVim/Neovim_from_scratch
  autopairs.setup({
    check_ts = true,
    ts_config = {
      lua = { "string", "source" },
      javascript = { "string", "template_string" },
      java = false,
    },
    disable_filetype = { "telescopeprompt", "spectre_panel" },
    fast_wrap = {
      map = "<m-e>",
      chars = { "{", "[", "(", '"', "'" },
      pattern = string.gsub([[ [%'%"%)%>%]%)%}%,] ]], "%s+", ""),
      offset = 0, -- offset from pattern match
      end_key = "$",
      keys = "qwertyuiopzxcvbnmasdfghjkl",
      check_comma = true,
      highlight = "pmenusel",
      highlight_grey = "linenr",
    },
  })

  local cmp_autopairs = require "nvim-autopairs.completion.cmp"
  local cmp_ok, cmp = pcall(require, "cmp")
  if not cmp_ok then
    return
  end
  cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done({ map_char = { tex = "" } }))

end

return M
