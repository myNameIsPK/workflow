local M = {}

local fn = vim.fn

function M.bootstrap_packer()
  local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
  if fn.empty(fn.glob(install_path)) > 0 then
    vim.notify "Downloading packer.nvim..."
    vim.notify(
      fn.system { "git", "clone", "https://github.com/wbthomason/packer.nvim", install_path }
    )
    vim.cmd "packadd! packer.nvim"
    require("packer").sync()
  end
  vim.cmd("autocmd BufWritePost plugins/init.lua PackerCompile")
end

local installed
---Check if a plugin is on the system not whether or not it is loaded
---@param plugin_name string
---@return boolean
function M.plugin_installed(plugin_name)
  if not installed then
    local dirs = fn.expand(fn.stdpath 'data' .. '/site/pack/packer/start/*', true, true)
    local opt = fn.expand(fn.stdpath 'data' .. '/site/pack/packer/opt/*', true, true)
    vim.list_extend(dirs, opt)
    installed = vim.tbl_map(function(path)
      return fn.fnamemodify(path, ':t')
    end, dirs)
  end
  return vim.tbl_contains(installed, plugin_name)
end

return M
