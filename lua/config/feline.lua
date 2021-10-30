local M = {}

local vi_mode_utils = require "feline.providers.vi_mode"
local lsp = require "feline.providers.lsp"

local colors = {
  bg = "#282828",
  black = "#282828",
  yellow = "#d8a657",
  cyan = "#89b482",
  oceanblue = "#45707a",
  green = "#a9b665",
  orange = "#e78a4e",
  violet = "#d3869b",
  magenta = "#c14a4a",
  white = "#a89984",
  fg = "#a89984",
  skyblue = "#7daea3",
  red = "#ea6962",
}

-- local vi_mode_colors = {
--   NORMAL = "green",
--   OP = "green",
--   INSERT = "skyblue",
--   VISUAL = "yellow",
--   BLOCK = "yellow",
--   REPLACE = "violet",
--   ["V-REPLACE"] = "violet",
--   ENTER = "cyan",
--   MORE = "cyan",
--   SELECT = "orange",
--   COMMAND = "green",
--   SHELL = "green",
--   TERM = "green",
--   NONE = "skyblue"
-- }

local vi_mode_colors = {
  NORMAL = colors.green,
  OP = colors.green,
  INSERT = colors.skyblue,
  VISUAL = colors.yellow,
  BLOCK = colors.yellow,
  REPLACE = colors.violet,
  ["V-REPLACE"] = colors.violet,
  ENTER = colors.cyan,
  MORE = colors.cyan,
  SELECT = colors.orange,
  COMMAND = colors.green,
  SHELL = colors.green,
  TERM = colors.green,
  NONE = colors.skyblue
}

local icons = {
  linux = " ",
  macos = " ",
  windows = " ",

  errs = " ",
  warns = " ",
  infos = " ",
  hints = " ",
  -- errs = " ",
  -- hints = " ",

  added = "ﰂ ",
  removed = " ",
  changed = " ",

  lsp = " ",
  git = ""
}

local properties = {
  force_inactive = {
    filetypes = {
      "NvimTree",
      "dbui",
      "packer",
      "startify",
      "fugitive",
      "fugitiveblame"
    },
    buftypes = {"terminal"},
    bufnames = {}
  }
}

local function file_osinfo()
  local os = vim.bo.fileformat:upper()
  local icon
  if os == "UNIX" then
    icon = icons.linux
  elseif os == "MAC" then
    icon = icons.macos
  else
    icon = icons.windows
  end
  -- return icon .. os
  return icon
end

local function lsp_diagnostics_info()
  return {
    errs = lsp.get_diagnostics_count("Error"),
    warns = lsp.get_diagnostics_count("Warning"),
    infos = lsp.get_diagnostics_count("Information"),
    hints = lsp.get_diagnostics_count("Hint")
  }
end

local function diag_enable(f, s)
  return function()
    local diag = f()[s]
    return diag and diag ~= 0
  end
end

local function diag_of(f, s)
  local icon = icons[s]
  return function()
    local diag = f()[s]
    return icon .. diag
  end
end

local function vimode_hl()
  return {
    name = vi_mode_utils.get_mode_highlight_name(),
    fg = vi_mode_utils.get_mode_color()
  }
end

local comps = {
  vi_mode = {
    left = {
      provider = "▊",
      hl = vimode_hl,
      right_sep = " "
    },
    right = {
      provider = "▊",
      hl = vimode_hl,
      left_sep = " "
    }
  },
  file = {
    info = {
      provider = "file_info",
      hl = {
        fg = colors.white,
        -- style = "bold"
      }
    },
    encoding = {
      provider = "file_encoding",
      left_sep = " ",
      hl = {
        fg = colors.white,
        -- style = "bold"
      }
    },
    type = {
      provider = "file_type",
      hl = {
        fg = colors.white,
        style = 'bold'
      }
    },
    position = {
      provider = 'position',
      left_sep = ' ',
      hl = {
        fg = colors.white,
        -- style = 'bold'
      }
    },
    os = {
      provider = file_osinfo,
      left_sep = " ",
      hl = {
        fg = colors.white,
        -- style = "bold"
      }
    }
  },
  line_percentage = {
    provider = "line_percentage",
    left_sep = " ",
    hl = {
      fg = colors.white,
      -- style = "bold"
    }
  },
  scroll_bar = {
    provider = "scroll_bar",
    left_sep = " ",
    hl = {
      fg = colors.yellow,
      -- style = "bold"
    }
  },
  diagnos = {
    err = {
      provider = diag_of(lsp_diagnostics_info, "errs"),
      left_sep = " ",
      enabled = diag_enable(lsp_diagnostics_info, "errs"),
      hl = {
        fg = colors.red
      }
    },
    warn = {
      provider = diag_of(lsp_diagnostics_info, "warns"),
      left_sep = " ",
      enabled = diag_enable(lsp_diagnostics_info, "warns"),
      hl = {
        fg = colors.yellow
      }
    },
    info = {
      provider = diag_of(lsp_diagnostics_info, "infos"),
      left_sep = " ",
      enabled = diag_enable(lsp_diagnostics_info, "infos"),
      hl = {
        fg = colors.skyblue
      }
    },
    hint = {
      provider = diag_of(lsp_diagnostics_info, "hints"),
      left_sep = " ",
      enabled = diag_enable(lsp_diagnostics_info, "hints"),
      hl = {
        fg = colors.cyan
      }
    },
  },
  lsp = {
    name = {
      provider = "lsp_client_names",
      left_sep = " ",
      icon = icons.lsp,
      hl = {
        fg = colors.white,
        style = "bold"
      }
    }
  },
  git = {
    branch = {
      provider = "git_branch",
      icon = icons.git,
      left_sep = " ",
      hl = {
        fg = colors.white,
        style = "bold"
      },
    },
    add = {
      provider = "git_diff_added",
      icon = icons.added,
      left_sep = " ",
      hl = {
        fg = colors.green
      }
    },
    change = {
      provider = "git_diff_changed",
      icon = icons.changed,
      left_sep = " ",
      hl = {
        fg = colors.orange
      }
    },
    remove = {
      provider = "git_diff_removed",
      icon = icons.removed,
      left_sep = " ",
      hl = {
        fg = colors.red
      }
    }
  }
}

local _components = {
  left = {
    active = {
      comps.vi_mode.left,
      comps.file.info,
      comps.git.branch,
      comps.git.add,
      comps.git.change,
      comps.git.remove,
      comps.lsp.name,
      comps.diagnos.hint,
      comps.diagnos.warn,
      comps.diagnos.err,
      comps.diagnos.info,
    },
    inactive = {
      comps.file.info,
    }
  },
  mid = {
    active = {},
    inactive = {}
  },
  right = {
    active = {
      comps.file.position,
      comps.file.encoding,
      comps.file.os,
      comps.file.type,
      comps.line_percentage,
      comps.scroll_bar,
      -- comps.vi_mode.right,
    },
    inactive = {}
  }
}

local components = {
  active = {
    _components.left.active,
    _components.mid.active,
    _components.right.active,
  },
  inactive = {
    _components.left.inactive,
    _components.mid.inactive,
    _components.right.inactive,
  }
}

function M.setup()
  require("feline").setup({
    -- preset = "noicon",
    -- colors = colors,
    default_bg = colors.bg,
    default_fg = colors.fg,
    components = components,
    properties = properties,
    vi_mode_colors = vi_mode_colors,
  })
end

return M
