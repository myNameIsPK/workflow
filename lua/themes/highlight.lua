-- vim: fmr={{{,}}} fdm=marker:
-- Init {{{
local c = require("themes.colors")
local hi = require("utils.highlight").highlight
local link = require("utils.highlight").hi_link

c.bg0 = c.dark0_soft
c.bg1 = c.dark1
c.bg2 = c.dark2
c.bg3 = c.dark3
c.bg4 = c.dark4

c.fg0 = c.light0_soft
c.fg1 = c.light1
c.fg2 = c.light2
c.fg3 = c.light3
c.fg4 = c.light4

c.gray   = c.gray_245

c.red    = c.bright_red
c.green  = c.bright_green
c.yellow = c.bright_yellow
c.blue   = c.bright_blue
c.purple = c.bright_purple
c.aqua   = c.bright_aqua
c.orange = c.bright_orange

hi('Fg0', { fg = c.fg0 })
hi('Fg1', { fg = c.fg1 })
hi('Fg2', { fg = c.fg2 })
hi('Fg3', { fg = c.fg3 })
hi('Fg4', { fg = c.fg4 })

hi('Bg0', { bg = c.bg0 })
hi('Bg1', { bg = c.bg1 })
hi('Bg2', { bg = c.bg2 })
hi('Bg3', { bg = c.bg3 })
hi('Bg4', { bg = c.bg4 })

hi('Gray',   { fg = c.gray   }, { "Grey" })
hi('Red',    { fg = c.red    })
hi('Orange', { fg = c.orange })
hi('Yellow', { fg = c.yellow })
hi('Green',  { fg = c.green  })
hi('Aqua',   { fg = c.aqua   })
hi('Blue',   { fg = c.blue   })
hi('Purple', { fg = c.purple })

hi('GrayBold',   { fg = c.gray,   style = 'bold' })
hi('RedBold',    { fg = c.red,    style = 'bold' })
hi('OrangeBold', { fg = c.orange, style = 'bold' })
hi('YellowBold', { fg = c.yellow, style = 'bold' })
hi('GreenBold',  { fg = c.green,  style = 'bold' })
hi('AquaBold',   { fg = c.aqua,   style = 'bold' })
hi('BlueBold',   { fg = c.blue,   style = 'bold' })
hi('PurpleBold', { fg = c.purple, style = 'bold' })

hi('GrayItalic',   { fg = c.gray,   style = 'italic' })
hi('RedItalic',    { fg = c.red,    style = 'italic' })
hi('OrangeItalic', { fg = c.orange, style = 'italic' })
hi('YellowItalic', { fg = c.yellow, style = 'italic' })
hi('GreenItalic',  { fg = c.green,  style = 'italic' })
hi('AquaItalic',   { fg = c.aqua,   style = 'italic' })
hi('BlueItalic',   { fg = c.blue,   style = 'italic' })
hi('PurpleItalic', { fg = c.purple, style = 'italic' })

hi('GrayUnderline',   { fg = c.gray,   style = 'underline' })
hi('RedUnderline',    { fg = c.red,    style = 'underline' })
hi('OrangeUnderline', { fg = c.orange, style = 'underline' })
hi('YellowUnderline', { fg = c.yellow, style = 'underline' })
hi('GreenUnderline',  { fg = c.green,  style = 'underline' })
hi('AquaUnderline',   { fg = c.aqua,   style = 'underline' })
hi('BlueUnderline',   { fg = c.blue,   style = 'underline' })
hi('PurpleUnderline', { fg = c.purple, style = 'underline' })
-- }}}
-- UI {{{
hi("Normal", { fg = c.fg0, bg = c.bg0 })

hi("Cursor", { style = "reverse" }, {
  "vCursor",
  "iCursor",
  "lCursor",
  "CursorIM",
})

link("CursorLine", "Bg1", {
  "CursorColumn",
})
hi("ColorColumn", { bg = c.red })

link("CursorLineNr", "YellowBold")
link("LineNr", "Fg4")

link("SignColumn", "Bg0")

hi("Folded", { fg = c.gray, bg = c.bg2 }, {
  "FoldColumn"
})

link("Whitespace", "Gray", {
  "NonText",
  "EndOfBuffer",
  "SpecialKey",
})

hi("IncSearch", { fg = c.bg0, bg = c.red    })
hi("Search",    { fg = c.bg0, bg = c.yellow })

hi("Visual", { style = "reverse" })

hi("MatchParen", { style = "reverse" })

hi("StatusLine",   { bg = c.bg4 })
hi("StatusLineNC", { bg = c.bg1 })

hi("TabLine",     { bg = c.bg2 })
hi("TabLineSel",  { bg = c.bg4 })
hi("TabLineFill", { bg = c.bg0 })

hi("VertSplit", { fg = c.bg3, bg = c.bg0 })
-- }}}
-- Completion {{{
hi("Pmenu",      { fg = c.fg1,  bg = c.bg2   })
hi("PmenuSel",   { fg = c.bg2,  bg = c.gray, style = "bold" })
hi("WildMenu",   { fg = c.gray, bg = c.bg2,  style = "bold" })
hi("PmenuSbar",  { bg = c.bg2   })
hi("PmenuThumb", { bg = c.bg3   })
-- }}}
-- Message {{{
link("ErrorMsg",   "RedBold")
link("WarningMsg", "YellowBold")
link("ModeMsg",    "YellowBold")
link("MoreMsg",    "YellowBold")
link("Question",   "OrangeBold")
-- }}}
-- Diff {{{
link("DiffAdd",    "Green")
link("DiffChange", "Yellow")
link("DiffDelete", "Red")
link("DiffText",   "Blue")
link("Directory",  "Blue")
-- }}}
-- Spell {{{
hi("SpellBad",   { style = "undercurl", sp = c.red    })
hi("SpellCap",   { style = "undercurl", sp = c.yellow })
hi("SpellRare",  { style = "undercurl", sp = c.orange })
hi("SpellLocal", { style = "undercurl", sp = c.purple })
-- }}}
-- Syntax {{{
hi("Boolean",        { fg    = c.purple    })
hi("Number",         { fg    = c.purple    })
hi("Float",          { fg    = c.purple    })

hi("PreProc",        { fg    = c.purple,   style = "italic" })
hi("PreCondit",      { fg    = c.purple,   style = "italic" })
hi("Include",        { fg    = c.purple,   style = "italic" })
hi("Define",         { fg    = c.purple,   style = "italic" })
hi("Conditional",    { fg    = c.red,      style = "italic" })
hi("Repeat",         { fg    = c.red,      style = "italic" })
hi("Keyword",        { fg    = c.red,      style = "italic" })
hi("Typedef",        { fg    = c.red,      style = "italic" })
hi("Exception",      { fg    = c.red,      style = "italic" })
hi("Statement",      { fg    = c.red,      style = "italic" })

hi("Error",          { fg    = c.red       })
hi("StorageClass",   { fg    = c.orange    })
hi("Tag",            { fg    = c.orange    })
hi("Label",          { fg    = c.orange    })
hi("Structure",      { fg    = c.orange    })
hi("Operator",       { fg    = c.orange    })
hi("Title",          { fg    = c.orange,   style = "bold"   })
hi("Special",        { fg    = c.yellow    })
hi("SpecialChar",    { fg    = c.yellow    })
hi("Type",           { fg    = c.yellow    })

hi("Function",       { fg    = c.green,    style = "bold"   })

hi("String",         { fg    = c.green     })
hi("Character",      { fg    = c.green     })
hi("Constant",       { fg    = c.aqua      })
hi("Macro",          { fg    = c.aqua      })
hi("Identifier",     { fg    = c.blue      })

hi("Comment",        { fg    = c.gray,     style = "italic" })
hi("SpecialComment", { fg    = c.gray,     style = "italic" })
hi("Todo",           { fg    = c.purple,   style = "italic" })

hi("Delimiter",      { fg    = c.fg0       })
hi("Ignore",         { fg    = c.gray      })
hi("Underlined",     { style = "underline" })
-- }}}
-- Plugins {{{
-- Cmp {{{
hi("CmpItemAbbrMatch", { fg = c.green, style = "bold"})
hi("CmpItemAbbrMatchFuzzy", { fg = c.purple, style = "bold"})
link("CmpItemAbbr",              "Fg")
link("CmpItemAbbrDeprecated",    "Red")
link("CmpItemMenu",              "Gray")
link("CmpItemKind",              "Yellow")
link("CmpItemKindText",          "Gray")
link("CmpItemKindMethod",        "Green")
link("CmpItemKindFunction",      "Green")
link("CmpItemKindConstructor",   "Green")
link("CmpItemKindField",         "Green")
link("CmpItemKindVariable",      "Blue")
link("CmpItemKindClass",         "Yellow")
link("CmpItemKindInterface",     "Yellow")
link("CmpItemKindModule",        "Yellow")
link("CmpItemKindProperty",      "Blue")
link("CmpItemKindUnit",          "Purple")
link("CmpItemKindValue",         "Purple")
link("CmpItemKindEnum",          "Yellow")
link("CmpItemKindKeyword",       "Red")
link("CmpItemKindSnippet",       "Aqua")
link("CmpItemKindColor",         "Aqua")
link("CmpItemKindFile",          "Aqua")
link("CmpItemKindReference",     "Aqua")
link("CmpItemKindFolder",        "Aqua")
link("CmpItemKindEnumMember",    "Purple")
link("CmpItemKindConstant",      "Blue")
link("CmpItemKindStruct",        "Yellow")
link("CmpItemKindEvent",         "Orange")
link("CmpItemKindOperator",      "Orange")
link("CmpItemKindTypeParameter", "Yellow")
-- }}}
-- Treesitter {{{
hi("TSStrong", { style = "bold" })
hi("TSEmphasis", { style = "italic" })
hi("TSUnderline", { style = "underline" })
hi("TSTodo", { fg = c.fg0, bg = c.orange, style = "bold" })
hi("TSNote", { fg = c.fg0, bg = c.blue, style = "bold" })
hi("TSWarning", { fg = c.bg0, bg = c.yellow, style = "bold" })
hi("TSDanger", { fg = c.fg0, bg = c.red, style = "bold" })
link("TSAnnotation", "Purple")
link("TSAttribute", "Purple")
link("TSBoolean", "Purple")
link("TSCharacter", "Aqua")
link("TSComment", "Comment")
link("TSConditional", "Red")
link("TSConstBuiltin", "BlueItalic")
link("TSConstMacro", "BlueItalic")
link("TSConstant", "Fg")
link("TSConstructor", "GreenBold")
link("TSException", "Red")
link("TSField", "Green")
link("TSFloat", "Purple")
link("TSFuncBuiltin", "GreenBold")
link("TSFuncMacro", "GreenBold")
link("TSFunction", "GreenBold")
link("TSInclude", "Red")
link("TSKeyword", "Red")
link("TSKeywordFunction", "Red")
link("TSKeywordOperator", "Orange")
link("TSLabel", "Orange")
link("TSMethod", "GreenBold")
link("TSNamespace", "YellowItalic")
link("TSNone", "Fg")
link("TSNumber", "Purple")
link("TSOperator", "Orange")
link("TSParameter", "Fg")
link("TSParameterReference", "Fg")
link("TSProperty", "Fg")
link("TSPunctBracket", "Fg")
link("TSPunctDelimiter", "Gray")
link("TSPunctSpecial", "Blue")
link("TSRepeat", "Red")
link("TSStorageClass", "Orange")
link("TSString", "Aqua")
link("TSStringEscape", "Green")
link("TSStringRegex", "Green")
link("TSStructure", "BlueItalic")
link("TSSymbol", "Fg")
link("TSTag", "Orange")
link("TSTagDelimiter", "Green")
link("TSText", "Green")
link("TSStrike", "Gray")
link("TSMath", "Blue")
link("TSType", "Yellow")
link("TSTypeBuiltin", "YellowItalic")
link("TSURI", "BlueUnderline")
link("TSVariable", "Fg")
link("TSVariableBuiltin", "BlueItalic")
-- }}}
-- }}}
