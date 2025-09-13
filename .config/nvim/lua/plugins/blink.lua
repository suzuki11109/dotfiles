return {
  -- {
  --   "L3MON4D3/LuaSnip",
  --   version = "2.*",
  --   build = "make install_jsregexp",
  --   dependencies = {
  --     "rafamadriz/friendly-snippets",
  --   },
  --   config = function()
  --     require("luasnip.loaders.from_vscode").lazy_load()
  --   end,
  -- },
  {
    "saghen/blink.cmp",
    event = "InsertEnter",
    version = "1.*",
    dependencies = {
      "rafamadriz/friendly-snippets",
    },
    opts = {
      keymap = {
        preset = "super-tab",
        ["<CR>"] = { "accept", "fallback" },
      },

      -- snippets = { preset = "luasnip" },

      sources = {
        default = { "lsp", "lazydev", "snippets" },
        providers = {
          lazydev = { module = "lazydev.integrations.blink", score_offset = 100 },
        },
      },

      cmdline = {
        enabled = false,
      },

      completion = {
        -- trigger = {
        --   show_on_keyword = false,
        --   show_on_trigger_character = false,
        -- },
        list = {
          selection = {
            auto_insert = false,
          },
        },
        --   documentation = {
        --     auto_show = true,
        --     auto_show_delay_ms = 50,
        --     window = {
        --       max_height = 10,
        --       max_width = 60,
        --     },
        --   },
        -- },
        --
        -- signature = {
        --   enabled = true,
        --   trigger = {
        --     show_on_insert_on_trigger_character = true,
        --   },
        --   window = {
        --     max_height = 1,
        --   },
      },

      fuzzy = {
        implementation = "prefer_rust_with_warning",
        -- implementation = "lua",
        sorts = {
          "exact",
          "score",
          "sort_text",
        },
      },
    },
  },
}
