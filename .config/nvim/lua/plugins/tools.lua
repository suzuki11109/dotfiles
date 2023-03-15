return {
   {
      'jghauser/mkdir.nvim',
      event = 'VeryLazy',
   },

   {
      'klen/nvim-test',
      opts = {
         termOpts = {
            width = 80,
            go_back = true,
         },
      },
      keys = {
         { '<leader>ta', ':TestSuite<cr>' },
         { '<leader>tf', ':TestFile<cr>' },
         { '<leader>tt', ':TestNearest<cr>' },
         { '<leader>tl', ':TestLast<cr>' },
         { '<leader>tb', ':TestVisit<cr>' },
      },
   },

   {
      'is0n/jaq-nvim',
      config = function()
         require('jaq-nvim').setup {
            cmds = {
               external = {
                  go = 'go run %',
               },
            },
            behavior = {
               default = 'bang',
            },
         }
      end,
      cmd = 'Jaq',
      keys = {
         { '<leader>rf', '<cmd>Jaq<cr>' },
      },
   },

   {
      'iamcco/markdown-preview.nvim',
      event = 'VeryLazy',
      build = function()
         vim.fn['mkdp#util#install']()
      end,
   },

   {
      'aserowy/tmux.nvim',
      event = 'VeryLazy',
      config = function()
         return require('tmux').setup {
            copy_sync = {
               enable = false,
            },
         }
      end,
   },

   {
      'akinsho/toggleterm.nvim',
      version = '*',
      event = 'VeryLazy',
      config = function()
         require('toggleterm').setup {
            open_mapping = [[<C-\>]],
            size = function(term)
               if term.direction == 'horizontal' then
                  return 17
               elseif term.direction == 'vertical' then
                  return vim.o.columns * 0.3
               end
            end,
         }

         function _G.set_terminal_keymaps()
            local opts = { buffer = 0 }
            vim.keymap.set('t', '<esc>', [[<C-\><C-n>]], opts)
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
      keys = {
         { '<leader>g]', '<cmd>Gitsigns next_hunk<cr>' },
         { '<leader>g[', '<cmd>Gitsigns prev_hunk<cr>' },
         { '<leader>gh', '<cmd>Gitsigns preview_hunk<cr>' },
         { '<leader>gs', '<cmd>Gitsigns stage_hunk<cr>' },
         { '<leader>gu', '<cmd>Gitsigns undo_stage_hunk<cr>' },
         { '<leader>gx', '<cmd>Gitsigns reset_stage_hunk<cr>' },
      },
   },

   {
      'tpope/vim-fugitive',
      cmd = 'Git',
      keys = {
         { '<leader>gg', '<cmd>Git<cr>' },
         { '<leader>gt', '<cmd>Git status<cr>' },
         { '<leader>gP', '<cmd>Git push<cr>' },
         { '<leader>gF', '<cmd>Git pull --rebase<cr>' },
         { '<leader>gz', '<cmd>Git stash<cr>' },
         { '<leader>gZ', '<cmd>Git stash pop<cr>' },
         { '<leader>gb', '<cmd>Git branch<cr>' },
      },
   },
}
