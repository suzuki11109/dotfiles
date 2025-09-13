return {
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    config = function()
      require("nvim-autopairs").setup({
        check_ts = true,
      })
    end,
  },
  {
    "kylechui/nvim-surround",
    version = "^3.0.0",
    event = "InsertEnter",
    config = function()
      require("nvim-surround").setup({
        aliases = {
          ["'"] = { '"', "'", "`" },
        },
      })
    end,
  },
  {
    "stevearc/conform.nvim",
    event = { "BufWritePre" },
    cmd = { "ConformInfo" },
    keys = {
      {
        "<leader>cf",
        function()
          require("conform").format({ async = true, lsp_format = "fallback" })
        end,
      },
    },
    opts = {
      format_on_save = function(bufnr)
        local disable_filetypes = { c = true, cpp = true }
        if disable_filetypes[vim.bo[bufnr].filetype] then
          return nil
        else
          return {
            timeout_ms = 500,
            lsp_format = "fallback",
          }
        end
      end,
      formatters_by_ft = {
        lua = { "stylua" },
        go = { "goimports" },
        -- python = { "isort", "black" },
        -- javascript = { "prettierd", "prettier", stop_after_first = true },
      },
    },
  },
}
