local map = require("core.utils").map

vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

map({ "n", "v" }, "<space>", "<nop>", { silent = true })

map("", ";", ":")

map({ "n", "v" }, "<leader>bn", ":bnext<CR>")
map({ "n", "v" }, "<leader>bp", ":bprevious<CR>")

map({ "n", "v" }, "<C-k>", ":wincmd k<CR>")
map({ "n", "v" }, "<C-j>", ":wincmd j<CR>")
map({ "n", "v" }, "<C-h>", ":wincmd h<CR>")
map({ "n", "v" }, "<C-l>", ":wincmd l<CR>")

map("v", "<", "<gv") -- indenting stay in visual mode
map("v", ">", ">gv")

map("v", "p", '"_dP') -- keep last yank
