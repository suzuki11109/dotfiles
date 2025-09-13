local map = require("core.utils").map

return {
  "stevearc/oil.nvim",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  lazy = false,
  config = function()
    require("oil").setup({
      default_file_explorer = true,
      columns = {
        "icon",
        "permission",
        "size",
        "mtime",
      },
      keymaps = {
        ["q"] = "actions.close",
      },
      delete_to_trash = true,
      view_options = {
        show_hidden = true,
      },
    })
    map("n", "-", "<cmd>Oil<cr>")
  end,
}
