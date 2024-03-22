local M = {}

local start
local ok
local warn
local error
if vim.fn.has "nvim-0.10.0" == 1 then
  start = vim.health.start
  ok = vim.health.ok
  warn = vim.health.warn
  error = vim.health.error
else
  start = vim.health.report_start
  ok = vim.health.report_ok
  warn = vim.health.report_warn
  error = vim.health.report_error
end

function M.check()
  start "My Personal Health Check"
  if vim.fn.has "nvim-0.9" == 1 then
    ok "Neovim version >= 0.9"
  else
    error "You should use Neovim >= 0.9"
  end

  local ver_info = vim.version()
  if ver_info and ver_info.prerelease then
    ok("Build From Source 0." .. ver_info.minor)
  else
    warn "Not Build From Source"
  end

  start "Needed dependencies"
  local exec_deps = my.exec_deps
  for _, exec in ipairs(exec_deps) do
    if vim.fn.executable(exec) then
      ok("Has '" .. exec .. "' executable")
    else
      error("No '" .. exec .. "' executable")
    end
  end

  start "Optional dependencies"
  local opt_exec_deps = my.opt_exec_deps
  for _, exec in ipairs(opt_exec_deps) do
    if vim.fn.executable(exec) == 1 then
      ok("Has '" .. exec .. "' executable")
    else
      error("No '" .. exec .. "' executable")
    end
  end

  start "Local configs override"
  if my.got_override then
    ok "Detect local settings: got overrided"
  else
    ok "Undetect local settings: notthing change from settings"
  end
end

return M
