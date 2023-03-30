local has_words_before = function()
   unpack = unpack or table.unpack
   local line, col = unpack(vim.api.nvim_win_get_cursor(0))
   return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match '%s' == nil
end

return {
   {
      'L3MON4D3/LuaSnip',
      version = '1.*',
      build = 'make install_jsregexp',
      dependencies = {
         'rafamadriz/friendly-snippets',
      },
      -- opts = {
      -- history = true,
      -- delete_check_events = 'TextChanged',
      -- },
      -- keys = {
      --    {
      --       '<tab>',
      --       function()
      --          return require('luasnip').jumpable(1) and '<Plug>luasnip-jump-next' or '<tab>'
      --       end,
      --       expr = true,
      --       silent = true,
      --       mode = 'i',
      --    },
      --    {
      --       '<tab>',
      --       function()
      --          require('luasnip').jump(1)
      --       end,
      --       mode = 's',
      --    },
      --    {
      --       '<s-tab>',
      --       function()
      --          require('luasnip').jump(-1)
      --       end,
      --       mode = { 'i', 's' },
      --    },
      -- },
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
      config = function()
         local cmp = require 'cmp'
         local luasnip = require 'luasnip'
         require('luasnip.loaders.from_vscode').lazy_load()

         cmp.setup {
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
            -- completion = {
            --    keyword_length = 2,
            -- },
            mapping = cmp.mapping.preset.insert {
               ['<C-d>'] = cmp.mapping.scroll_docs(-4),
               ['<C-f>'] = cmp.mapping.scroll_docs(4),
               ['<C-Space>'] = cmp.mapping.complete(),
               -- ['<Tab>'] = cmp.mapping.confirm {
               --    behavior = cmp.ConfirmBehavior.Replace,
               --    select = true,
               -- },
               ['<CR>'] = cmp.mapping.confirm {
                  behavior = cmp.ConfirmBehavior.Replace,
                  select = true,
               },
               ['<Tab>'] = cmp.mapping(function(fallback)
                  if cmp.visible() then
                     cmp.confirm {
                        behavior = cmp.ConfirmBehavior.Replace,
                        select = true,
                     }
                  -- You could replace the expand_or_jumpable() calls with expand_or_locally_jumpable()
                  -- they way you will only jump inside the snippet region
                  elseif luasnip.expand_or_jumpable() then
                     luasnip.expand_or_jump()
                  elseif has_words_before() then
                     cmp.complete()
                  else
                     fallback()
                  end
               end, { 'i', 's' }),
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

         -- Set configuration for specific filetype.
         cmp.setup.filetype('lua', {
            sources = cmp.config.sources {
               { name = 'nvim_lsp' },
            },
         })

         cmp.setup.filetype('rust', {
            sources = cmp.config.sources {
               { name = 'nvim_lsp' },
            },
         })

         -- cmp.setup.cmdline(':', {
         --    mapping = cmp.mapping.preset.cmdline(),
         --    sources = cmp.config.sources({
         --       { name = 'path' },
         --    }, {
         --       {
         --          name = 'cmdline',
         --          option = {
         --             ignore_cmds = { 'Man', '!' },
         --          },
         --       },
         --    }),
         -- })
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
      event = 'VeryLazy',
      config = function()
         require('nvim-surround').setup {
            aliases = {
               ["'"] = { '"', "'", '`' },
            },
         }
      end,
   },

   {
      'numToStr/Comment.nvim',
      event = { 'BufReadPre', 'BufNewFile' },
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
      cmd = 'HopChar2',
      branch = 'v2',
      config = function()
         return require('hop').setup {}
      end,
      -- keys = {
      --    { 's', ':HopChar2<cr>' },
      -- },
   },
}
