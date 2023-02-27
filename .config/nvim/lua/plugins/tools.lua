return {
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
      'ahmedkhalf/project.nvim',
      event = { 'BufReadPre', 'BufNewFile' },
      dependencies = {
         'nvim-telescope/telescope.nvim',
      },
      config = function()
         require('project_nvim').setup {
            exclude_dirs = {
               '/home/aki',
            },
         }

         require('telescope').load_extension 'projects'
      end,
      keys = {
         { '<leader>pp', '<cmd>Telescope projects<cr>' },
      },
   },

   {
      'jghauser/mkdir.nvim',
      event = 'VeryLazy',
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
      keys = {
         { '<leader>rf', '<cmd>Jaq<cr>' },
      },
   },
}
