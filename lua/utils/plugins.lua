local M = {}

local fn = vim.fn

function M.bootstrap_lazy()
  local lazypath = vim.fn.stdpath "data" .. "/lazy/lazy.nvim"
  if not vim.loop.fs_stat(lazypath) then
    vim.fn.system {
      "git",
      "clone",
      "--filter=blob:none",
      "https://github.com/folke/lazy.nvim.git",
      "--branch=stable",
      lazypath,
    }
  end
  vim.opt.rtp:prepend(lazypath)
end

---Check if a plugin is on the system not whether or not it is loaded
---@param plugin_name string
---@return boolean
function M.plugin_installed(plugin_name)
  if not my.installed_plugins then
    local dirs = fn.expand(fn.stdpath "data" .. "/site/pack/packer/start/*", true, true)
    vim.list_extend(dirs, fn.expand(fn.stdpath "data" .. "/site/pack/packer/opt/*", true, true))
    vim.list_extend(dirs, fn.expand(fn.stdpath "data" .. "/lazy/*", true, true))
    vim.list_extend(dirs, fn.expand(vim.env.HOME .. "/.local/src/nvim-plugins/*", true, true))
    my.installed_plugins = vim.tbl_map(function(path)
      return fn.fnamemodify(path, ":t")
    end, dirs)
  end
  return vim.tbl_contains(my.installed_plugins, plugin_name)
end

return M
