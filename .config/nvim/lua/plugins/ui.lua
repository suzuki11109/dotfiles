return {
   {
      'nvim-telescope/telescope.nvim',
      branch = '0.1.x',
      dependencies = { 'nvim-lua/plenary.nvim' },
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
      keys = {
         { '<leader><space>', '<cmd>Telescope commands<cr>', desc = 'commands' },
         { '<leader>.', '<cmd>Telescope resume<cr>', desc = 'resume' },
         { '<leader>:', '<cmd>Telescope command_history<cr>', desc = 'command history' },
         { '<leader>ff', '<cmd>Telescope find_files cwd=%:p:h<cr>', desc = 'find current dir files' },
         { '<leader>pf', '<cmd>Telescope find_files<cr>', desc = 'find files' },
         { '<leader>fr', '<cmd>Telescope oldfiles<cr>', desc = 'recent files' },
         { '<leader>ss', '<cmd>Telescope live_grep<cr>', desc = 'live grep' },
         { '<leader>sl', '<cmd>Telescope current_buffer_fuzzy_find<cr>', desc = 'search this buffer' },
         { '<leader>bb', '<cmd>Telescope buffers sort_lastused=true ignore_current_buffer=true<cr>', desc = 'switch buffers' },
         { '<leader>hh', '<cmd>Telescope help_tags<cr>', desc = 'search help' },
         { '<leader>hk', '<cmd>Telescope keymaps<cr>', desc = 'keymaps' },
         { '<leader>hl', '<cmd>Telescope highlights<cr>', desc = 'highlights' },
         { '<leader>gb', '<cmd>Telescope git_branches<cr>', desc = 'git branches' },
      },
   },

   { -- Fuzzy Finder Algorithm which requires local dependencies to be built. Only load if `make` is available
      'nvim-telescope/telescope-fzf-native.nvim',
      build = 'make',
      dependencies = { 'nvim-telescope/telescope.nvim' },
      cond = vim.fn.executable 'make' == 1,
      config = function()
         require('telescope').load_extension 'fzf'
      end,
   },

   {
      'smilovanovic/telescope-search-dir-picker.nvim',
      event = 'VimEnter',
      dependencies = { 'nvim-telescope/telescope.nvim' },
      config = function()
         require('telescope').load_extension 'search_dir_picker'
      end,
      keys = {
         { '<leader>sd', '<cmd>Telescope search_dir_picker<cr>', desc = 'search in directory' },
      },
   },

   {
      'ahmedkhalf/project.nvim',
      event = 'VimEnter',
      dependencies = {
         'nvim-telescope/telescope.nvim',
      },
      config = function()
         require('project_nvim').setup {
            detection_methods = { 'pattern', 'lsp' },
            patterns = { '.git', '_darcs', '.hg', '.bzr', '.svn', 'go.mod', 'build.sbt', 'pyproject.toml', 'Makefile', 'package.json' },
            ignore_lsp = { 'null-ls' },
            exclude_dirs = {
               '/home/aki',
            },
            silent_chdir = false,
         }

         require('telescope').load_extension 'projects'
      end,
      keys = {
         { '<leader>pp', '<cmd>Telescope projects<cr>' },
      },
   },

   {
      'google/vim-searchindex',
      event = 'VeryLazy',
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
      dependencies = 'nvim-tree/nvim-web-devicons',
      config = function()
         require('trouble').setup {
            height = 10,
            padding = false,
         }
      end,
      keys = {
         { '<leader>cx', '<cmd>TroubleToggle<cr>' },
      },
   },

   {
      'nvim-tree/nvim-tree.lua',
      dependencies = {
         'nvim-tree/nvim-web-devicons', -- optional, for file icons
      },
      config = function()
         require('nvim-tree').setup {
            sync_root_with_cwd = true,
            respect_buf_cwd = true,
            update_focused_file = {
               enable = true,
               update_root = true,
            },
         }
      end,
      keys = {
         { '<leader>on', '<cmd>NvimTreeToggle<cr>', { desc = 'nvim tree' } },
      },
   },

   {
      'NvChad/nvim-colorizer.lua',
      event = 'VeryLazy',
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
}
