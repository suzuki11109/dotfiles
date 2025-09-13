return {
  "catppuccin/nvim",
  name = "catppuccin",
  priority = 1000,
  config = function()
    require("catppuccin").setup({
      integrations = {
        blink_cmp = true,
        fidget = true,
        flash = true,
        mason = true,
        neotest = true,
        lsp_trouble = true,
        native_lsp = {
          enabled = true,
          underlines = {
            errors = { "undercurl" },
            hints = { "undercurl" },
            warnings = { "undercurl" },
            information = { "undercurl" },
          },
        },
        telescope = true,
        treesitter = true,
      },
    })
    vim.cmd.colorscheme("catppuccin")
  end,
}
