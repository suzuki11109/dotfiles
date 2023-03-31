return {
   {
      'nvim-telescope/telescope.nvim',
      branch = '0.1.x',
      cmd = 'Telescope',
      dependencies = {
         'nvim-telescope/telescope-fzf-native.nvim',
      },
      config = function()
         local actions = require 'telescope.actions'
         require('telescope').setup {
            defaults = {
               file_ignore_patterns = { '.git/', 'node_modules/' },
               layout_config = {
                  preview_width = 0.50,
                  prompt_position = 'top',
               },
               path_display = { 'smart' },
               sorting_strategy = 'ascending',
               mappings = {
                  i = {
                     ['<C-u>'] = false,
                     ['<C-d>'] = false,
                     ['<esc>'] = actions.close,
                     ['kj'] = actions.close,
                     ['<C-g>'] = actions.close,
                     ['<C-f>'] = actions.preview_scrolling_down,
                     ['<C-b>'] = actions.preview_scrolling_up,
                  },
               },
            },
            pickers = {
               find_files = {
                  find_command = { 'rg', '--files', '--hidden' },
               },
            },
         }
      end,
   },

   { -- Fuzzy Finder Algorithm which requires local dependencies to be built. Only load if `make` is available
      'nvim-telescope/telescope-fzf-native.nvim',
      build = 'make',
      cond = vim.fn.executable 'make' == 1,
      config = function()
         require('telescope').load_extension 'fzf'
      end,
   },

   {
      'princejoogie/dir-telescope.nvim',
      config = function()
         require('dir-telescope').setup {
            hidden = true,
            respect_gitignore = true,
         }

         require('telescope').load_extension 'dir'
      end,
   },

   {
      'google/vim-searchindex',
      event = 'CmdlineEnter',
   },

   {
      'stevearc/dressing.nvim',
      event = 'VeryLazy',
      config = function()
         require('dressing').setup()
      end,
   },

   {
      'folke/trouble.nvim',
      cmd = 'TroubleToggle',
      dependencies = 'nvim-tree/nvim-web-devicons',
      config = function()
         require('trouble').setup {
            height = 15,
            padding = false,
         }
      end,
   },

   {
      'NvChad/nvim-colorizer.lua',
      event = { 'BufReadPre', 'BufNewFile' },
      name = 'colorizer',
      opts = {
         user_default_options = {
            names = false,
         },
      },
   },

   {
      'nvim-treesitter/nvim-treesitter',
      event = { 'BufReadPre', 'BufNewFile' },
      dependencies = { 'nvim-treesitter/nvim-treesitter-textobjects' },
      build = function()
         pcall(require('nvim-treesitter.install').update { with_sync = true })
      end,
      opts = {
         ensure_installed = 'all',
         highlight = { enable = true },
         indent = { enable = true },
         textobjects = {
            select = {
               enable = true,
               lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
               keymaps = {
                  ['aa'] = '@parameter.outer',
                  ['ia'] = '@parameter.inner',
                  ['af'] = '@function.outer',
                  ['if'] = '@function.inner',
                  ['ac'] = '@class.outer',
                  ['ic'] = '@class.inner',
               },
            },
            move = {
               enable = true,
               set_jumps = true,
               goto_next_start = {
                  [']m'] = '@function.outer',
                  [']]'] = '@class.outer',
               },
               goto_next_end = {
                  [']M'] = '@function.outer',
                  [']['] = '@class.outer',
               },
               goto_previous_start = {
                  ['[m'] = '@function.outer',
                  ['[['] = '@class.outer',
               },
               goto_previous_end = {
                  ['[M'] = '@function.outer',
                  ['[]'] = '@class.outer',
               },
            },
         },
      },
      config = function(_, opts)
         require('nvim-treesitter.configs').setup(opts)
      end,
   },

   {
      'cbochs/grapple.nvim',
      cmd = { 'GrappleSelect', 'GrapplePopup', 'GrappleTag', 'GrappleUntag', 'GrappleCycle' },
      opts = {
         popup_options = {
            -- width = vim.api.nvim_win_get_width(0) - 10,
            border = 'rounded',
            height = 17,
         },
      },
   },

   {
      'stevearc/oil.nvim',
      cmd = 'Oil',
      opts = {
         columns = {
            'icon',
            'permission',
            'size',
            'mtime',
         },
         win_options = {
            conceallevel = 3,
            concealcursor = 'nv',
         },
         keymaps = {
            ['g?'] = 'actions.show_help',
            ['q'] = 'actions.close',
         },
         view_options = {
            show_hidden = true,
         },
      },
   },
   {
      'folke/which-key.nvim',
      config = function()
         -- vim.o.timeout = true
         -- vim.o.timeoutlen = 300
         require('which-key').setup {
            window = {
               border = 'rounded',
            },
            plugins = {
               marks = false,
            },
         }
      end,
   },
}
