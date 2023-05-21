# Globals variable `my` override

When you want to setting difference globals options in local machine create file `lua/override_globals.lua` to override it.

```lua
-- ./lua/override_globals.lua
my.opts.colorscheme.default = "ron"
```

This file is ignore by `git`.
