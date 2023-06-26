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
   },

   {
      'hrsh7th/nvim-cmp',
      event = 'InsertEnter',
      dependencies = {
         'hrsh7th/cmp-nvim-lsp',
         'L3MON4D3/LuaSnip',
         'saadparwaiz1/cmp_luasnip',
         'onsails/lspkind.nvim',
         'windwp/nvim-autopairs',
      },
      config = function()
         local cmp = require 'cmp'
         local luasnip = require 'luasnip'
         require('luasnip.loaders.from_vscode').lazy_load()

         cmp.setup {
            completion = {
               keyword_length = 2,
            },
            preselect = cmp.PreselectMode.None,
            window = {
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
               ['<C-e>'] = cmp.mapping.abort(),
               ['<C-Space>'] = cmp.mapping.complete(),
               ['<CR>'] = cmp.mapping.confirm { select = true },
               ['<Tab>'] = cmp.mapping(function(fallback)
                  if cmp.visible() then
                     cmp.select_next_item()
                  elseif luasnip.expand_or_jumpable() then
                     luasnip.expand_or_jump()
                  elseif has_words_before() then
                     cmp.complete()
                  else
                     fallback()
                  end
               end, { 'i', 's' }),

               ['<S-Tab>'] = cmp.mapping(function(fallback)
                  if cmp.visible() then
                     cmp.select_prev_item()
                  elseif luasnip.jumpable(-1) then
                     luasnip.jump(-1)
                  else
                     fallback()
                  end
               end, { 'i', 's' }),
               -- ['<Tab>'] = cmp.mapping(function(fallback)
               --    if cmp.visible() then
               --       cmp.confirm {
               --          behavior = cmp.ConfirmBehavior.Replace,
               --          select = true,
               --       }
               --    elseif luasnip.expand_or_jumpable() then
               --       luasnip.expand_or_jump()
               --    elseif has_words_before() then
               --       cmp.complete()
               --    else
               --       fallback()
               --    end
               -- end),
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

         local cmp_autopairs = require 'nvim-autopairs.completion.cmp'
         cmp.event:on('confirm_done', cmp_autopairs.on_confirm_done())
      end,
   },

   {
      'kylechui/nvim-surround',
      event = 'InsertEnter',
      config = function()
         require('nvim-surround').setup {
            aliases = {
               ["'"] = { '"', "'", '`' },
            },
         }
      end,
   },

   {
      'windwp/nvim-autopairs',
      config = function()
         require('nvim-autopairs').setup {}
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
      'ggandor/leap.nvim',
      event = { 'BufReadPre', 'BufNewFile' },
      config = function()
         require('leap').add_default_mappings()
      end,
   },
}
