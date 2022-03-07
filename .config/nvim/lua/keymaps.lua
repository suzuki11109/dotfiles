local function map(mode, lhs, rhs, opts)
	local options = { noremap = true, silent = true }
	if opts then
		options = vim.tbl_extend("force", options, opts)
	end
	vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

local opt = {}

map("", ";", ":", { silent = false })

-- visual line
map("", "j", 'v:count ? "j" : "gj"', { expr = true })
map("", "k", 'v:count ? "k" : "gk"', { expr = true })

-- delete blank
map("n", "dd", [[match(getline('.'), '^\s*$') == -1 ? 'dd' : '"_dd']], { expr = true })
map("n", "cc", [[match(getline('.'), '^\s*$') == -1 ? 'cc' : '"_cc']], { expr = true })

map("n", "Q", "<Nop>", opt)
map("n", "q:", "<Nop>", opt)

map("v", ">", ">gv", opt)
map("v", "<", "<gv", opt)

map("i", "<C-u>", "<C-g>u<C-u>", opt)
map("n", "n", "nzz", opt)
map("n", "N", "Nzz", opt)
map("n", "*", "*zz", opt)
map("n", "#", "#zz", opt)
map("n", "<C-o>", "<C-o>zz", opt)

map("n", "J", "mzJ`z", opt)

-- window
map("n", "<Leader>wj", "<C-w>j", opt)
map("n", "<Leader>wk", "<C-w>k", opt)
map("n", "<Leader>wh", "<C-w>h", opt)
map("n", "<Leader>wl", "<C-w>l", opt)
map("n", "<Leader>wo", "<C-w>o", opt)
map("n", "<Leader>ws", ":split<CR>", opt)
map("n", "<Leader>wv", ":vsplit<CR>", opt)

-- buffer
map("n", "<Leader>bd", ":bp|bd#<CR>", opt)
map("n", "<Leader>bn", ":bnext<CR>", opt)
map("n", "<Leader>bp", ":bprevious<CR>", opt)
map("n", "<Leader>bl", ":b#<CR>", opt)

-- map("n", "<C-h>", [[<C-W>h]], opt)
-- map("n", "<C-j>", [[<C-W>j]], opt)
-- map("n", "<C-k>", [[<C-W>k]], opt)
-- map("n", "<C-l>", [[<C-W>l]], opt)

-- create directory
map("n", "<Leader>fe", ":e <C-R>=expand('%:p:h') . '/'<CR>", { silent = false })
-----------------------------------------------------------
-- Applications & Plugins shortcuts:
-----------------------------------------------------------
-- open terminal
map('n', '<C-t>', ':Term<CR>', opt)

-- navigator
map("n", "<C-h>", "<CMD>lua require('Navigator').left()<CR>", opt)
map("n", "<C-k>", "<CMD>lua require('Navigator').up()<CR>", opt)
map("n", "<C-l>", "<CMD>lua require('Navigator').right()<CR>", opt)
map("n", "<C-j>", "<CMD>lua require('Navigator').down()<CR>", opt)

-- telescope
map("n", "<Leader><space>", ":Telescope commands<CR>", opt)
map("n", "<Leader>fr", ":Telescope oldfiles<CR>", opt)
map(
	"n",
	"<Leader>ff",
	"<cmd>lua require'telescope.builtin'.file_browser({cwd = vim.fn.expand('%:p:h'), hidden = true})<CR>",
	opt
)
map("n", "<Leader>pf", ":Telescope find_files<CR>", opt)
map("n", "<Leader>sp", ":Telescope live_grep<CR>", opt)
map("n", "<Leader>sh", ":Telescope highlight<CR>", opt)
map("n", "<Leader>st", ":Telescope colorscheme<CR>", opt)

map("n", "<Leader>bb", ":Telescope buffers<CR>", opt)
