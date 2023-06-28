return {
   {
      'nvim-lua/plenary.nvim',
      lazy = false,
   },
   {
      'jghauser/mkdir.nvim',
      event = 'CmdlineEnter',
   },

   {
      'klen/nvim-test',
      cmd = { 'TestSuite', 'TestFile', 'TestNearest', 'TestLast', 'TestVisit' },
      config = function()
         require('nvim-test').setup {
            term = 'toggleterm',
            termOpts = {
               direction = 'horizontal',
               height = vim.o.lines * 0.5,
               go_back = true,
            },
         }
      end,
   },

   {
      'akinsho/toggleterm.nvim',
      event = 'VeryLazy',
      config = function()
         require('toggleterm').setup {
            open_mapping = [[<C-\>]],
            size = function(term)
               if term.direction == 'horizontal' then
                  return vim.o.lines * 0.5
               elseif term.direction == 'vertical' then
                  return vim.o.columns * 0.3
               end
            end,
            winbar = {
               enabled = true,
               name_formatter = function(term) --  term: Terminal
                  return term.name
               end,
            },
         }

         function _G.set_terminal_keymaps()
            local opts = { buffer = 0 }
            vim.keymap.set('t', '<esc>', [[<C-\><C-n>]], opts)
            -- vim.keymap.set('t', 'kj', [[<C-\><C-n>]], opts)
            vim.keymap.set('t', '<C-h>', [[<Cmd>wincmd h<CR>]], opts)
            vim.keymap.set('t', '<C-j>', [[<Cmd>wincmd j<CR>]], opts)
            vim.keymap.set('t', '<C-k>', [[<Cmd>wincmd k<CR>]], opts)
            vim.keymap.set('t', '<C-l>', [[<Cmd>wincmd l<CR>]], opts)
            vim.keymap.set('t', '<C-w>', [[<C-\><C-n><C-w>]], opts)
         end

         vim.cmd 'autocmd! TermOpen term://* lua set_terminal_keymaps()'
      end,
   },

   {
      'willothy/flatten.nvim',
      lazy = false,
      priority = 1001,
      opts = {
         window = {
            open = 'alternate',
         },
         callbacks = {
            should_block = function(argv)
               return vim.tbl_contains(argv, '-b')
            end,
            post_open = function(bufnr, winnr, ft, is_blocking)
               if is_blocking then
                  require('toggleterm').toggle(0)
               else
                  vim.api.nvim_set_current_win(winnr)
               end

               -- If the file is a git commit, create one-shot autocmd to delete its buffer on write
               -- If you just want the toggleable terminal integration, ignore this bit
               if ft == 'gitcommit' then
                  vim.api.nvim_create_autocmd('BufWritePost', {
                     buffer = bufnr,
                     once = true,
                     callback = function()
                        -- This is a bit of a hack, but if you run bufdelete immediately
                        -- the shell can occasionally freeze
                        vim.defer_fn(function()
                           vim.api.nvim_buf_delete(bufnr, {})
                           require('toggleterm').toggle(0)
                        end, 100)
                     end,
                  })
               end
            end,
            -- block_end = function()
            --    require('toggleterm').toggle(0)
            -- end,
         },
      },
   },

   {
      'lewis6991/gitsigns.nvim',
      event = { 'BufReadPre', 'BufNewFile' },
      opts = {
         signs = {
            add = { text = '▎' },
            change = { text = '▎' },
            delete = { text = '契' },
            topdelete = { text = '契' },
            changedelete = { text = '▎' },
            untracked = { text = '▎' },
         },
      },
   },

   {
      'tpope/vim-fugitive',
      cmd = { 'Git', 'G' },
   },

   {
      'rest-nvim/rest.nvim',
      ft = 'http',
      config = function()
         require('rest-nvim').setup {
            result = {
               show_curl_command = false,
            },
            jump_to_request = false,
         }
      end,
   },
   {
      'stevearc/overseer.nvim',
      cmd = { 'OverseerToggle', 'OverseerRun', 'OverseerRunCmd' },
      opts = {
         strategy = 'toggleterm',
      },
   },
   {
      'akinsho/git-conflict.nvim',
      event = 'VeryLazy',
      version = '*',
      config = true,
   },

   { 'gpanders/editorconfig.nvim', event = 'VeryLazy' },
   {
      'm4xshen/hardtime.nvim',
      event = 'VeryLazy',
      opts = {},
   },
}
