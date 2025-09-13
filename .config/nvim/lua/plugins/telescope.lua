return {
  {
    "nvim-telescope/telescope.nvim",
    branch = "0.1.x",
    dependencies = {
      "nvim-lua/plenary.nvim",
      { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
      "jonarrien/telescope-cmdline.nvim",
      "nvim-tree/nvim-web-devicons",
    },
    config = function()
      local actions = require("telescope.actions")
      -- local themes = require("telescope.themes")
      require("telescope").setup({
        -- defaults = themes.get_ivy({
        --   file_ignore_patterns = { ".git/" },
        --   layout_config = { height = 0.35 },
        --   mappings = {
        --     i = {
        --       ["<esc>"] = actions.close,
        --     },
        --   },
        --   preview = false,
        --   results_title = "",
        -- }),
        defaults = {
          dynamic_preview_title = true,
          sorting_strategy = "ascending",
          file_ignore_patterns = { ".git/" },
          layout_config = {
            prompt_position = "top",
            height = 0.9,
            width = 0.9,
          },
          mappings = {
            i = {
              ["<esc>"] = actions.close,
            },
          },
          results_title = "",
        },
        extensions = {
          cmdline = {
            picker = {
              layout_config = {
                height = 0.9,
                width = 0.9,
              },
            },
          },
        },
      })

      require("telescope").load_extension("fzf")
      require("telescope").load_extension("cmdline")

      local builtin = require("telescope.builtin")
      vim.keymap.set("n", "<leader><space>", "<cmd>Telescope cmdline<cr>")
      vim.keymap.set("n", "<leader>bb", builtin.buffers)
      vim.keymap.set("n", "<leader>fr", builtin.oldfiles)
      vim.keymap.set("n", "<leader>pf", builtin.find_files)
      -- vim.keymap.set('n', '<leader>ff', builtin.find_files)
      vim.keymap.set("n", "<leader>sl", builtin.current_buffer_fuzzy_find)
      vim.keymap.set("n", "<leader>sp", builtin.live_grep)
      vim.keymap.set("n", "<leader>hh", builtin.help_tags)
    end,
  },
}
