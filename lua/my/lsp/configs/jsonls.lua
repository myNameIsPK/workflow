local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true

return {
  capabilities = capabilities,
  settings = {
    json = {
      validate = { enable = true },
      -- Schemas https://www.schemastore.org
      schemas = require("schemastore").json.schemas {
        -- select = {
        --   '.eslintrc',
        --   'package.json',
        -- },
        -- ignore = {
        --   '.eslintrc',
        --   'package.json',
        -- },
        -- replace = {
        --   ['package.json'] = {
        --     description = 'package.json overriden',
        --     fileMatch = { 'package.json' },
        --     name = 'package.json',
        --     url = 'https://example.com/package.json',
        --   },
        -- },
      },
      -- schemas = {
      --   {
      --     fileMatch = { 'package.json' },
      --     url = 'https://json.schemastore.org/package.json',
      --   },
      --   {
      --     fileMatch = { 'tsconfig*.json' },
      --     url = 'https://json.schemastore.org/tsconfig.json',
      --   },
      --   {
      --     fileMatch = {
      --       '.prettierrc',
      --       '.prettierrc.json',
      --       'prettier.config.json',
      --     },
      --     url = 'https://json.schemastore.org/prettierrc.json',
      --   },
      --   {
      --     fileMatch = { '.eslintrc', '.eslintrc.json' },
      --     url = 'https://json.schemastore.org/eslintrc.json',
      --   },
      --   {
      --     fileMatch = { '.babelrc', '.babelrc.json', 'babel.config.json' },
      --     url = 'https://json.schemastore.org/babelrc.json',
      --   },
      --   {
      --     fileMatch = { 'lerna.json' },
      --     url = 'https://json.schemastore.org/lerna.json',
      --   },
      --   {
      --     fileMatch = { 'now.json', 'vercel.json' },
      --     url = 'https://json.schemastore.org/now.json',
      --   },
      --   {
      --     fileMatch = {
      --       '.stylelintrc',
      --       '.stylelintrc.json',
      --       'stylelint.config.json',
      --     },
      --     url = 'http://json.schemastore.org/stylelintrc.json',
      --   },
      -- },
    },
  },
}
