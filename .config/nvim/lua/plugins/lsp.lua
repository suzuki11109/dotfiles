local map = require("core.utils").map

return {
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      { "mason-org/mason.nvim", opts = {} },
      "mason-org/mason-lspconfig.nvim",
      "WhoIsSethDaniel/mason-tool-installer.nvim",
      {
        "j-hui/fidget.nvim",
        opts = {
          notification = {
            window = {
              winblend = 0,
            },
          },
        },
      },
      "saghen/blink.cmp",
    },
    config = function()
      vim.api.nvim_create_autocmd("LspAttach", {
        group = vim.api.nvim_create_augroup("UserLspConfig", {}),
        callback = function(event)
          map({ "n" }, "<leader>cr", vim.lsp.buf.rename)
          map({ "n", "v" }, "<leader>ca", vim.lsp.buf.code_action)
          map({ "n" }, "gd", vim.lsp.buf.definition)
          map({ "n" }, "K", vim.lsp.buf.hover)
          local telescope = require("telescope.builtin")
          map({ "n" }, "<leader>cR", telescope.lsp_references)
          map({ "n" }, "<leader>ci", telescope.lsp_implementations)
          map({ "n" }, "<leader>cj", telescope.lsp_workspace_symbols)
          map({ "n" }, "<leader>cQ", "<cmd>LspRestart<CR>")
        end,
      })

      local servers = {
        ts_ls = {},
        gopls = {
          skip_install = true,
          settings = {
            gopls = {
              usePlaceholders = true,
            },
          },
        },
        lua_ls = {
          settings = {
            Lua = {
              completion = {
                callSnippet = "Replace",
              },
            },
          },
        },
      }

      -- diagnostics
      vim.diagnostic.config({
        severity_sort = true,
        -- float = { border = 'rounded', source = 'if_many' },
        -- underline = { severity = vim.diagnostic.severity.ERROR },
        signs = {
          text = {
            [vim.diagnostic.severity.ERROR] = "󰅚 ",
            [vim.diagnostic.severity.WARN] = "󰀪 ",
            [vim.diagnostic.severity.INFO] = "󰋽 ",
            [vim.diagnostic.severity.HINT] = "󰌶 ",
          },
        },
        -- virtual_text = {
        --     source = 'if_many',
        --     spacing = 2,
        --     format = function(diagnostic)
        --         local diagnostic_message = {
        --             [vim.diagnostic.severity.ERROR] = diagnostic.message,
        --             [vim.diagnostic.severity.WARN] = diagnostic.message,
        --             [vim.diagnostic.severity.INFO] = diagnostic.message,
        --             [vim.diagnostic.severity.HINT] = diagnostic.message,
        --         }
        --         return diagnostic_message[diagnostic.severity]
        --     end,
        -- },
      })
      map({ "n", "v" }, "]q", function()
        vim.diagnostic.goto_next({ float = false })
      end)
      map({ "n", "v" }, "[q", function()
        vim.diagnostic.goto_prev({ float = false })
      end)

      local capabilities = require("blink.cmp").get_lsp_capabilities({
        textDocument = {
          completion = {
            completionItem = {
              -- resolveSupport = {
              --   properties = {
              --     "documentation",
              --     "additionalTextEdits",
              --     "insertTextFormat",
              --     "insertTextMode",
              --     "command",
              --   },
              -- },
            },
          },
        },
      })

      local ensure_installed = {
        "stylua",
      }

      for name, config in pairs(servers) do
        if not config.skip_install then
          table.insert(ensure_installed, name)
        end
      end

      require("mason-tool-installer").setup({ ensure_installed = ensure_installed })

      require("mason-lspconfig").setup({
        ensure_installed = {},
        automatic_installation = false,
        handlers = {
          function(server_name)
            local server = servers[server_name] or {}
            server.capabilities = vim.tbl_deep_extend("force", {}, capabilities, server.capabilities or {})
            require("lspconfig")[server_name].setup(server)
          end,
        },
      })
    end,
  },
  {
    "folke/trouble.nvim",
    cmd = "Trouble",
    opts = {
      focus = true,
      open_no_results = true,
    },
    keys = {
      { "<leader>cx", "<cmd>Trouble diagnostics toggle<cr>" },
      { "<leader>cX", "<cmd>Trouble diagnostics toggle filter.buf=0<cr>" },
    },
  },
  {
    "folke/lazydev.nvim",
    ft = "lua",
    opts = {
      library = {
        { path = "${3rd}/luv/library", words = { "vim%.uv" } },
      },
    },
  },
  {
    "mrcjkb/rustaceanvim",
    version = "^6",
    lazy = false,
  },
  {
    "nvim-flutter/flutter-tools.nvim",
    lazy = false,
    config = function()
      require("flutter-tools").setup({
        widget_guides = {
          enabled = true,
        },
      })
    end,
  },
}
