return {
  "nvim-neotest/neotest",
  dependencies = {
    "nvim-neotest/nvim-nio",
    "nvim-lua/plenary.nvim",
    "antoinemadec/FixCursorHold.nvim",
    "nvim-treesitter/nvim-treesitter",
    { "fredrikaverpil/neotest-golang", version = "*" },
  },
  config = function()
    require("neotest").setup({
      adapters = {
        require("neotest-golang")({}),
      },
      output = { open_on_run = true },
    })
  end,
  keys = {
    {
      "<leader>tt",
      function()
        require("neotest").run.run()
        require("neotest").output_panel.open({ enter = true })
      end,
    },
    {
      "<leader>tf",
      function()
        require("neotest").run.run(vim.fn.expand("%"))
      end,
    },
    {
      "<leader>ta",
      function()
        require("neotest").run.run({ suite = true })
      end,
    },
  },
}
