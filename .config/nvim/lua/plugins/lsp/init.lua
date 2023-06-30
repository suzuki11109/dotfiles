local lsputil = require 'plugins.lsp.util'

return {
   {
      'neovim/nvim-lspconfig',
      enabled = true,
      event = { 'BufReadPre', 'BufNewFile' },
      dependencies = {
         {
            'folke/neodev.nvim',
            config = function()
               require('neodev').setup {}
            end,
         },
         'hrsh7th/cmp-nvim-lsp',
         'williamboman/mason.nvim',
         'williamboman/mason-lspconfig.nvim',
         'jose-elias-alvarez/null-ls.nvim',
         'j-hui/fidget.nvim',
      },
      opts = {
         servers = {
            jsonls = {},
            solargraph = {},
            gopls = {},
            angularls = {},
            pyright = {},
            kotlin_language_server = {},
            lua_ls = {
               settings = {
                  Lua = {
                     workspace = { checkThirdParty = false },
                     telemetry = { enable = false },
                     completion = { callSnippet = 'Replace' },
                  },
               },
            },
         },
      },
      config = function(_, opts)
         require('mason-lspconfig').setup()
         require('lspconfig.ui.windows').default_options.border = 'rounded'

         local servers = opts.servers
         local capabilities = vim.lsp.protocol.make_client_capabilities()
         capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

         local lspconfig = require 'lspconfig'
         for server, server_opts in pairs(servers) do
            local servopts = vim.tbl_deep_extend('force', {
               capabilities = vim.deepcopy(capabilities),
               on_attach = lsputil.on_attach,
            }, server_opts or {})
            lspconfig[server].setup(servopts)
         end
      end,
   },

   {
      'williamboman/mason.nvim',
      cmd = 'Mason',
      build = ':MasonUpdate',
      config = function()
         require('mason').setup {
            ui = {
               border = 'rounded',
            },
         }
      end,
   },
   {
      'jose-elias-alvarez/null-ls.nvim',
      opts = function()
         local nls = require 'null-ls'
         return {
            sources = {
               nls.builtins.formatting.prettierd,
               nls.builtins.formatting.stylua,
               nls.builtins.formatting.goimports,
               nls.builtins.diagnostics.golangci_lint,
            },
         }
      end,
   },

   {
      'amrbashir/nvim-docs-view',
      commit = '601d7f6a2e399226a669fd2a73f7da77726cb32f',
      cmd = { 'DocsViewToggle', 'DocsViewUpdate' },
      config = function()
         require('docs-view').setup {
            update_mode = 'manual',
            position = 'bottom',
            height = vim.o.lines * 0.5 - 1,
         }
      end,
   },

   {
      'j-hui/fidget.nvim',
      tag = 'legacy',
      config = function()
         require('fidget').setup {
            window = {
               blend = 0,
            },
         }
      end,
   },

   {
      'scalameta/nvim-metals',
      ft = { 'scala', 'sbt' },
      config = function()
         local metals_config = require('metals').bare_config()
         metals_config.settings = {
            showImplicitArguments = true,
            excludedPackages = { 'akka.actor.typed.javadsl', 'com.github.swagger.akka.javadsl' },
         }
         metals_config.init_options.statusBarProvider = 'on'
         metals_config.handlers['metals/status'] = lsputil.metals_status_handler
         local capabilities = vim.lsp.protocol.make_client_capabilities()
         metals_config.capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)
         metals_config.on_attach = lsputil.on_attach
         local nvim_metals_group = vim.api.nvim_create_augroup('nvim-metals', { clear = true })

         vim.api.nvim_create_autocmd('FileType', {
            pattern = { 'scala', 'sbt', 'java' },
            callback = function()
               require('metals').initialize_or_attach(metals_config)
            end,
            group = nvim_metals_group,
         })
      end,
   },

   {
      'simrat39/rust-tools.nvim',
      ft = 'rust',
      config = function()
         require('rust-tools').setup {
            server = {
               on_attach = lsputil.on_attach,
            },
         }
      end,
   },
   {
      'jose-elias-alvarez/typescript.nvim',
      ft = { 'typescript', 'typescriptreact', 'javscript' },
      config = function()
         require('typescript').setup {
            server = {
               on_attach = lsputil.on_attach,
            },
         }
      end,
   },
}
