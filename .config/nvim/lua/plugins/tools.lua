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
      opts = {
         term = 'toggleterm',
         termOpts = {
            direction = 'horizontal',
            height = 15,
            go_back = true,
         },
      },
   },

   {
      'iamcco/markdown-preview.nvim',
      ft = 'markdown',
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
   },

   {
      'sindrets/diffview.nvim',
      cmd = {
         'DiffviewFileHistory',
         'DiffviewOpen',
         'DiffviewToggleFiles',
      },
      config = function()
         local actions = require 'diffview.actions'
         require('diffview').setup {
            file_panel = {
               win_config = {
                  position = 'bottom',
                  height = 15,
               },
            },
            keymaps = {
               file_panel = {
                  { 'n', 'j', actions.select_next_entry, { desc = 'Open the diff for the next file' } },
                  { 'n', 'k', actions.select_prev_entry, { desc = 'Open the diff for the previous file' } },
                  { 'n', 'q', '<cmd>DiffviewClose<CR>', { desc = 'Close Diffview' } },
               },
            },
         }
      end,
   },
   {
      'tpope/vim-fugitive',
      cmd = { 'Git', 'G' },
   },
}
