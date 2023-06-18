return {
   {
      'nvim-lualine/lualine.nvim',
      event = 'VeryLazy',
      opts = {
         options = {
            theme = 'catppuccin',
            component_separators = { left = '', right = '' },
            section_separators = { left = '', right = '' },
         },
         sections = {
            lualine_a = { 'mode' },
            lualine_b = { 'branch' },
            lualine_c = { 'filename', 'location' },
            lualine_x = { 'filetype' },
            lualine_y = { 'diagnostics' },
            lualine_z = { 'overseer' },
         },
      },
   },
   {
      'nvim-telescope/telescope.nvim',
      tag = '0.1.1',
      cmd = 'Telescope',
      dependencies = {
         'nvim-telescope/telescope-fzf-native.nvim',
         'prochri/telescope-all-recent.nvim',
         'window-picker',
      },
      config = function()
         local actions = require 'telescope.actions'
         require('telescope').setup {
            defaults = {
               file_ignore_patterns = { '.git/', 'node_modules/' },
               layout_config = {
                  width = 0.9,
                  prompt_position = 'top',
                  horizontal = {
                     preview_width = 0.5,
                  },
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
                     ['<C-o>'] = function(prompt_bufnr)
                        -- Use nvim-window-picker to choose the window by dynamically attaching a function
                        local action_set = require 'telescope.actions.set'
                        local action_state = require 'telescope.actions.state'

                        local picker = action_state.get_current_picker(prompt_bufnr)
                        picker.get_selection_window = function(picker, entry)
                           local picked_window_id = require('window-picker').pick_window() or vim.api.nvim_get_current_win()
                           -- Unbind after using so next instance of the picker acts normally
                           picker.get_selection_window = nil
                           return picked_window_id
                        end

                        return action_set.edit(prompt_bufnr, 'edit')
                     end,
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
      'prochri/telescope-all-recent.nvim',
      dependencies = {
         'kkharji/sqlite.lua',
      },
      config = function()
         require('telescope-all-recent').setup {
            pickers = {
               find_files = {
                  sorting = 'recent',
               },
            },
         }
      end,
   },

   {
      'princejoogie/dir-telescope.nvim',
      config = function()
         require('dir-telescope').setup {
            hidden = true,
         }

         require('telescope').load_extension 'dir'
      end,
   },

   {
      's1n7ax/nvim-window-picker',
      name = 'window-picker',
      version = '2.*',
      config = function()
         require('window-picker').setup {
            hint = 'floating-big-letter',
            filter_rules = {
               include_current_win = true,
            },
         }
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
