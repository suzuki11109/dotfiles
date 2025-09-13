vim.o.number = true
vim.o.relativenumber = true
vim.o.clipboard = "unnamedplus"
-- vim.o.updatetime = 250
-- vim.o.timeoutlen = 300
vim.o.splitright = true
vim.o.splitbelow = true
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.expandtab = true
vim.o.shiftwidth = 2
vim.o.tabstop = 2
vim.o.softtabstop = 2
vim.o.list = true
vim.o.listchars = "tab:  ,trail:·,eol: ,nbsp:_"
vim.o.inccommand = "split" -- live substitution preview
vim.o.swapfile = false
vim.o.conceallevel = 0
vim.o.fileencoding = "utf-8"
vim.o.signcolumn = "yes"

-- disable new line comment
vim.api.nvim_create_autocmd("FileType", {
  pattern = "*",
  callback = function()
    vim.opt.formatoptions:remove({ "c", "r", "o" })
  end,
})
