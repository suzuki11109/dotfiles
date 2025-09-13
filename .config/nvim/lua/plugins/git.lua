return {
  {
    "FabijanZulj/blame.nvim",
    cmd = "BlameToggle",
    opts = {
      blame_options = { "-w" },
    },
    keys = {
      { "<leader>gB", "<cmd>BlameToggle<cr>" },
    },
  },
  {
    "NeogitOrg/neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
    },
    config = true,
    keys = {
      { "<leader>gg", "<cmd>Neogit<cr>" },
    },
  },
}
