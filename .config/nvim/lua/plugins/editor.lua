return {
   {
      'L3MON4D3/LuaSnip',
      dependencies = {
         'rafamadriz/friendly-snippets',
      },
      opts = {
         -- history = true,
         delete_check_events = 'TextChanged',
      },
      keys = {
         {
            '<tab>',
            function()
               return require('luasnip').jumpable(1) and '<Plug>luasnip-jump-next' or '<tab>'
            end,
            expr = true,
            silent = true,
            mode = 'i',
         },
         {
            '<tab>',
            function()
               require('luasnip').jump(1)
            end,
            mode = 's',
         },
         {
            '<s-tab>',
            function()
               require('luasnip').jump(-1)
            end,
            mode = { 'i', 's' },
         },
      },
   },

   {
      'hrsh7th/nvim-cmp',
      event = 'InsertEnter',
      dependencies = {
         'hrsh7th/cmp-nvim-lsp',
         'L3MON4D3/LuaSnip',
         'saadparwaiz1/cmp_luasnip',
         'onsails/lspkind.nvim',
      },
      opts = function()
         local cmp = require 'cmp'
         local luasnip = require 'luasnip'
         require('luasnip.loaders.from_vscode').lazy_load()

         return {
            preselect = cmp.PreselectMode.None,
            window = {
               -- completion = cmp.config.window.bordered(),
               documentation = cmp.config.window.bordered(),
            },
            snippet = {
               expand = function(args)
                  luasnip.lsp_expand(args.body)
               end,
            },
            mapping = cmp.mapping.preset.insert {
               ['<C-d>'] = cmp.mapping.scroll_docs(-4),
               ['<C-f>'] = cmp.mapping.scroll_docs(4),
               ['<C-Space>'] = cmp.mapping.complete(),
               ['<Tab>'] = cmp.mapping.confirm {
                  behavior = cmp.ConfirmBehavior.Replace,
                  select = true,
               },
               ['<CR>'] = cmp.mapping.confirm {
                  behavior = cmp.ConfirmBehavior.Replace,
                  select = true,
               },
            },
            sources = {
               { name = 'luasnip' },
               { name = 'nvim_lsp' },
            },
            formatting = {
               format = require('lspkind').cmp_format {
                  mode = 'symbol_text',
                  maxwidth = 60,
                  ellipsis_char = '...',
               },
            },
         }
      end,
   },

   {
      'windwp/nvim-autopairs',
      event = 'InsertEnter',
      config = function()
         require('nvim-autopairs').setup {}
      end,
   },

   {
      'kylechui/nvim-surround',
      event = 'CursorMoved',
      config = true,
   },

   {
      'numToStr/Comment.nvim',
      event = 'CursorMoved',
      dependencies = { 'JoosepAlviste/nvim-ts-context-commentstring' },
      config = function()
         require('Comment').setup {
            pre_hook = require('ts_context_commentstring.integrations.comment_nvim').create_pre_hook(),
         }
      end,
   },

   {
      'ethanholz/nvim-lastplace',
      event = { 'BufReadPre', 'BufNewFile' },
      opts = {
         lastplace_ignore_buftype = { 'quickfix', 'nofile', 'help' },
         lastplace_ignore_filetype = { 'gitcommit', 'gitrebase', 'svn', 'hgcommit' },
         lastplace_open_folds = true,
      },
   },

   {
      'max397574/better-escape.nvim',
      event = 'InsertEnter',
      opts = {
         mapping = { 'kj' },
      },
   },

   {
      'phaazon/hop.nvim',
      branch = 'v2',
      config = function()
         return require('hop').setup {}
      end,
      keys = {
         { 's', ':HopChar2<cr>' },
      },
   },
}
