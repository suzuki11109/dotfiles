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
      dependencies = { 'nvim-telescope/telescope.nvim' },
      config = function()
         require('dir-telescope').setup {
            hidden = true,
            respect_gitignore = true,
         }

         require('telescope').load_extension 'dir'
      end,
      -- keys = {
      --    {
      --       '<leader>sd',
      --       '<cmd>Telescope dir live_grep<CR>',
      --    },
      -- },
   },

   -- {
   --    'ahmedkhalf/project.nvim',
   --    event = 'VeryLazy',
   --    dependencies = {
   --       'nvim-telescope/telescope.nvim',
   --    },
   --    config = function()
   --       require('project_nvim').setup {
   --          detection_methods = { 'pattern', 'lsp' },
   --          patterns = { '.git', '_darcs', '.hg', '.bzr', '.svn', 'go.mod', 'build.sbt', 'pyproject.toml', 'Makefile', 'package.json' },
   --          ignore_lsp = { 'null-ls' },
   --          exclude_dirs = {
   --             '/home/aki',
   --          },
   --          silent_chdir = false,
   --       }
   --
   --       require('telescope').load_extension 'projects'
   --    end,
   --    -- keys = {
   --    --    { '<leader>pp', '<cmd>Telescope projects<cr>' },
   --    -- },
   -- },

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
            height = 10,
            padding = false,
         }
      end,
      -- keys = {
      --    { '<leader>cx', '<cmd>TroubleToggle<cr>' },
      -- },
   },

   -- {
   --    'nvim-tree/nvim-tree.lua',
   --    dependencies = {
   --       'nvim-tree/nvim-web-devicons', -- optional, for file icons
   --    },
   --    config = function()
   --       require('nvim-tree').setup {
   --          sync_root_with_cwd = true,
   --          respect_buf_cwd = true,
   --          update_focused_file = {
   --             enable = true,
   --             update_root = true,
   --          },
   --       }
   --    end,
   --    keys = {
   --       { '<leader>on', '<cmd>NvimTreeToggle<cr>', { desc = 'nvim tree' } },
   --    },
   -- },

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

   -- {
   --    'ThePrimeagen/harpoon',
   --    event = 'VeryLazy',
   --    dependencies = { 'nvim-lua/plenary.nvim' },
   --    config = function()
   --       require('harpoon').setup {}
   --    end,
   --    keys = {
   --       {
   --          '<leader>kk',
   --          function()
   --             require('harpoon.ui').toggle_quick_menu()
   --          end,
   --       },
   --       {
   --          '<leader>ki',
   --          function()
   --             require('harpoon.mark').add_file()
   --          end,
   --       },
   --       {
   --          '<leader>kn',
   --          function()
   --             require('harpoon.ui').nav_next()
   --          end,
   --       },
   --       {
   --          '<leader>kp',
   --          function()
   --             require('harpoon.ui').nav_prev()
   --          end,
   --       },
   --       -- { '<leader>:', '<cmd>Telescope command_history<cr>' },
   --    },
   -- },
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
      -- keys = {
      --    { '<leader>kk', '<cmd>GrapplePopup tags<CR>' },
      --    { '<leader>ki', '<cmd>GrappleTag<CR>' },
      --    { '<leader>ku', '<cmd>GrappleUntag<CR>' },
      --    { '<leader>kn', '<cmd>GrappleCycle forward<CR>' },
      --    { '<leader>kp', '<cmd>GrappleCycle backward<CR>' },
      --    { '<leader>k1', '<cmd>GrappleSelect key=1<CR>' },
      --    { '<leader>k2', '<cmd>GrappleSelect key=2<CR>' },
      --    { '<leader>k3', '<cmd>GrappleSelect key=3<CR>' },
      --    { '<leader>k4', '<cmd>GrappleSelect key=4<CR>' },
      --    { '<leader>k5', '<cmd>GrappleSelect key=5<CR>' },
      --    { '<leader>k6', '<cmd>GrappleSelect key=6<CR>' },
      --    { '<leader>k7', '<cmd>GrappleSelect key=7<CR>' },
      --    { '<leader>k8', '<cmd>GrappleSelect key=8<CR>' },
      --    { '<leader>k9', '<cmd>GrappleSelect key=9<CR>' },
      --    { '<leader>k0', '<cmd>GrappleSelect key=10<CR>' },
      -- },
   },
   -- {
   --    'cbochs/portal.nvim',
   --    dependencies = {
   --       'cbochs/grapple.nvim',
   --    },
   --    opts = {
   --       window_options = {
   --          height = 5,
   --          border = 'rounded',
   --       },
   --       escape = {
   --          ['<esc>'] = true,
   --          ['q'] = true,
   --       },
   --    },
   --    keys = {
   --       {
   --          '<leader>kl',
   --          function()
   --             require('portal.builtin').grapple.tunnel()
   --          end,
   --       },
   --       {
   --          '<leader>kj',
   --          function()
   --             require('portal.builtin').jumplist.tunnel()
   --          end,
   --       },
   --    },
   -- },
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
