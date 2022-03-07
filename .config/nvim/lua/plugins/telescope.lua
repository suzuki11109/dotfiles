
local actions = require("telescope.actions")

require("telescope").setup({
	defaults = {
		mappings = {
			i = {
				["<esc>"] = actions.close,
				["kj"] = actions.close,
			},
		},
	},
	pickers = {
		find_files = {
			find_command = { "rg", "--files", "--iglob", "!.git", "--hidden" },
		},
		file_browser = {
			hidden = true,
		},
	},
})
