local fn = vim.fn

local install_path = fn.stdpath("data") .. "/site/pack/packer/opt/packer.nvim"

if fn.empty(fn.glob(install_path)) > 0 then
	print("packer.nvim not found in " .. install_path .. ", installing ...")
	fn.system({ "git", "clone", "https://github.com/wbthomason/packer.nvim", install_path })
	execute("packadd packer.nvim")
end

vim.cmd([[packadd packer.nvim]])

vim.cmd([[autocmd BufWritePost plugins.lua source <afile> | PackerCompile]])

return require("packer").startup({
	function(use)
		use({ "wbthomason/packer.nvim", opt = true })
		use({ "nvim-lua/plenary.nvim" })
		use({ "nvim-lua/popup.nvim" })

		use({ "tpope/vim-unimpaired", event = { "CursorHold" } })
		use({ "tpope/vim-surround", event = { "CursorHold" } })

		use({
			"kyazdani42/nvim-web-devicons",
		})

	  use 'folke/tokyonight.nvim'
    use({
	    "catppuccin/nvim",
	    as = "catppuccin"
    })

		use({
			"jdhao/better-escape.vim",
			event = "InsertEnter",
			setup = function()
        vim.g.better_escape_interval = 300
        vim.g.better_escape_shortcut = {"kj"}
			end,
		})

	  use 'ishan9299/modus-theme-vim'

		use({
			"sheerun/vim-polyglot",
		})

		use({
			"nvim-treesitter/nvim-treesitter",
			run = ":TSUpdate",
			config = function()
				require("plugins.treesitter")
			end,
		})

		use({
			"hrsh7th/nvim-cmp",
			event = "InsertEnter",
			wants = "LuaSnip",
			requires = {
				{
					"L3MON4D3/LuaSnip",
					wants = "friendly-snippets",
					event = "InsertCharPre",
					config = function()
            require('luasnip').config.set_config({
                history = true,
                updateevents = "TextChanged,TextChangedI"
              })
            require("luasnip/loaders/from_vscode").load()
					end,
				},
				{
					"rafamadriz/friendly-snippets",
					event = "InsertCharPre",
				},
				{
					"saadparwaiz1/cmp_luasnip",
					wants = "LuaSnip",
					event = "InsertCharPre",
				},
				{
					"hrsh7th/cmp-nvim-lsp",
				},
			},
			config = function()
				require("plugins.cmp")
			end,
		})

		use({
			"nvim-telescope/telescope.nvim",
			event = "BufWinEnter",
			config = function()
				require("plugins.telescope")
			end,
		})

		use({
			"terrortylor/nvim-comment",
			event = "CursorHold",
			config = function()
				require("nvim_comment").setup()
			end,
		})

		use({
			"tpope/vim-fugitive",
			cmd = { "Git" },
		})

		use({
			"airblade/vim-rooter",
			event = "VimEnter",
		})

		use({
			"numToStr/Navigator.nvim",
			event = "CursorHold",
			config = function()
				require("Navigator").setup()
			end,
		})
	end,
	config = {
		display = {
			non_interactive = os.getenv("PACKER_NON_INTERACTIVE") or false,
			open_fn = function()
				return require("packer.util").float({ border = "rounded" })
			end,
		},
	},
})
