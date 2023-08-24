local zk = require "zk"

local opts = {
  -- can be "telescope", "fzf" or "select" (`vim.ui.select`)
  -- it's recommended to use "telescope" or "fzf"
  picker = "telescope",

  lsp = {
    -- `config` is passed to `vim.lsp.start_client(config)`
    config = {
      cmd = { "zk", "lsp" },
      name = "zk",
      -- on_attach = ...
      -- etc, see `:h vim.lsp.start_client()`
    },

    -- automatically attach buffers in a zk notebook that match the given filetypes
    auto_attach = {
      enabled = true,
      filetypes = { "markdown" },
    },
  },
}

local map = require("utils.mappings").map
local api = require "zk.api"
local notepath = vim.env.ZK_NOTEBOOK_DIR .. "/zettels"
local journalpath = vim.env.ZK_NOTEBOOK_DIR .. "/journal/daily"
---Create note in the new buffer(not saving the file yet)
--
---@param note_opts table New nots options but has `dryRun = true` by default
---@see https://github.com/mickael-menu/zk/blob/main/docs/editors-integration.md#zknew
local function create_new_note(note_opts)
  note_opts = note_opts or {}
  note_opts.dir = note_opts.dir or notepath
  -- insertLinkAtLocation not work with dryRun
  note_opts.dryRun = not note_opts.insertLinkAtLocation

  api.new(nil, note_opts, function(err, res)
    assert(not err, tostring(err))
    local content = vim.split(string.gsub(res.content, "\n$", ""), "\n")
    if vim.fn.filereadable(res.path) == 1 then
      vim.cmd.edit(res.path)
    else
      vim.cmd.edit(res.path)
      vim.api.nvim_buf_set_lines(0, -2, -1, true, content)
    end
  end)
end

---@return number[] tuple tuple of tuple of mark "<" and ">" ((r, c), (r, c))
local function get_range()
  local a = vim.api.nvim_buf_get_mark(0, "<")
  local b = vim.api.nvim_buf_get_mark(0, ">")
  return a, b
end

local function get_lsp_range(a, b)
  local location = {}
  location.uri = vim.lsp.util.make_given_range_params().textDocument.uri
  location.range = {
    start = { line = a[1] - 1, character = a[2] },
    ["end"] = { line = b[1] - 1, character = b[2] + 1 },
  }
  return location
end

---return string from selected range
---@param get_line boolean|nil get text by line or not
---@param a number[] tuple
---@param b number[] tuple
---@return string
local function get_selected_text(a, b, get_line)
  local str_table = {}
  if get_line then
    str_table = vim.api.nvim_buf_get_lines(0, a[1] - 1, b[1], true)
  else
    str_table = vim.api.nvim_buf_get_text(0, a[1] - 1, a[2], b[1] - 1, b[2] + 1, {})
  end
  if #str_table == 1 then
    return str_table[1]
  end
  local str = ""
  for _, txt in pairs(str_table) do
    str = str .. txt .. "\n"
  end
  return string.gsub(str, "\n$", "")
end

map("n", "<leader>nn", function()
  create_new_note { title = vim.fn.input("Create Note At " .. notepath .. "\nTitle: ") }
end, { desc = "ZK Create new note" })

map("n", "<leader>ndd", function()
  create_new_note { dir = journalpath }
end, { desc = "ZK Create today note" })

map("n", "<leader>nf", function()
  zk.edit { sort = { "modified" } }
end, { desc = "ZK search notes" })
map("v", "<leader>nf", ":'<,'>ZkMatch<CR>")

map("n", "<leader>ndf", function()
  zk.edit { sort = { "modified" }, hrefs = { "journal/daily" } }
end, { desc = "ZK search daily note" })

map("n", "<leader>nt", "<Cmd>ZkTags<CR>")
map("n", "<leader>nc", ":edit $ZK_NOTEBOOK_DIR/.zk/config.toml<CR>")

local function zk_keymaps(bufnr)
  local function map_buf(mode, lhs, rhs, desc, map_opts)
    map_opts = map_opts or {}
    map_opts = vim.tbl_extend("force", map_opts, { buffer = bufnr, desc = desc })
    vim.keymap.set(mode, lhs, rhs, map_opts)
  end

  map_buf("n", "<CR>", vim.lsp.buf.definition)

  map_buf("n", "<leader>nn", function()
    create_new_note {
      title = vim.fn.input "Creating Note\nTitle: ",
    }
  end, "ZK Create new Note")

  map_buf("v", "<leader>nnt", function()
    local a, b = get_range()
    create_new_note {
      title = get_selected_text(a, b),
    }
  end, "Zk new notes from title", { expr = true })

  map_buf("v", "<leader>nnT", function()
    local a, b = get_range()
    local location = get_lsp_range(a, b)
    vim.print(location)
    create_new_note {
      title = get_selected_text(a, b),
      insertLinkAtLocation = location,
    }
  end, "Zk new notes from title (insert link)", { expr = true })

  map_buf("v", "<leader>nnc", function()
    local a, b = get_range()
    create_new_note {
      title = vim.fn.input "Creating Note\nTitle: ",
      content = get_selected_text(a, b, true),
    }
  end, "Zk new notes from content", { expr = true })

  map_buf("v", "<leader>nnC", function()
    local a, b = get_range()
    local location = get_lsp_range(a, b)
    create_new_note {
      title = vim.fn.input "Creating Note\nTitle: ",
      content = get_selected_text(a, b, true),
      insertLinkAtLocation = location,
    }
  end, "Zk new notes from content (insert link)", { expr = true })

  map_buf("n", "<leader>nb", function()
    zk.edit({ linkTo = { vim.api.nvim_buf_get_name(0) } }, { title = "Zk Backlinks" })
  end, "Zk Backlinks")

  map_buf("n", "<leader>nl", function()
    zk.edit({ linkedBy = { vim.api.nvim_buf_get_name(0) } }, { title = "Zk Links" })
  end, "Zk Links")

  map_buf("v", "<leader>na", ":'<,'>lua vim.lsp.buf.code_action()<CR>")
end

opts.lsp.config.on_attach = function(_, bufnr)
  zk_keymaps(bufnr)
end

zk.setup(opts)
