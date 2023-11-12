return {
   {
      'google/vim-searchindex',
      event = 'CmdlineEnter',
   },

   {
      'nvim-telescope/telescope.nvim',
      tag = '0.1.4',
      cmd = 'Telescope',
      dependencies = {
         'nvim-telescope/telescope-fzf-native.nvim',
      },
      config = function()
         local actions = require 'telescope.actions'
         require('telescope').setup {
            defaults = {
               file_ignore_patterns = { '.git/' },
               results_title = '',
               layout_config = {
                  prompt_position = 'top',
                  preview_width = 0.5,
               },
               cache_picker = {
                  num_pickers = 20,
               },
               sorting_strategy = 'ascending',
               dynamic_preview_title = true,
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

   -- {
   --    'princejoogie/dir-telescope.nvim',
   --    config = function()
   --       require('dir-telescope').setup {
   --          hidden = true,
   --       }
   --
   --       require('telescope').load_extension 'dir'
   --    end,
   -- },

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
            padding = false,
         }
      end,
   },

   {
      'nvim-treesitter/nvim-treesitter',
      event = { 'BufReadPre', 'BufNewFile' },
      dependencies = { 
         'nvim-treesitter/nvim-treesitter-textobjects',
         'JoosepAlviste/nvim-ts-context-commentstring',
      },
      build = function()
         pcall(require('nvim-treesitter.install').update { with_sync = true })
      end,
      opts = {
         ensure_installed = {
            'go',
            'gomod',
            'gosum',
            'gowork',
            'git_config',
            'gitcommit',
            'css',
            'html',
            'markdown',
            'lua',
            'python',
            'ruby',
            'rust',
            'scala',
            'make',
            'dockerfile',
            'http',
            'terraform',
            'javascript',
            'typescript',
            'tsx',
            'vue',
            'json',
            'yaml',
            'toml',
            'sql',
         },
         highlight = { enable = true },
         indent = { enable = true },
         context_commentstring = {
           enable = true,
         }, 
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
         },
      },
      config = function(_, opts)
         require('nvim-treesitter.configs').setup(opts)
      end,
   },

   {
      'stevearc/oil.nvim',
      cmd = 'Oil',
      config = function()
         require('oil').setup {
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
         }
         vim.api.nvim_create_autocmd('FileType', {
            pattern = 'oil',
            command = 'setlocal winbar=%f',
         })
      end,
   },

   {
      'folke/which-key.nvim',
      config = function()
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
