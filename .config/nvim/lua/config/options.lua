-- [[ Setting options ]]

-- Set <space> as the leader key
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- Disable features
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

-- Decrease update time
vim.o.updatetime = 250
vim.wo.signcolumn = 'yes'

-- Make line numbers default
vim.wo.number = true
vim.o.relativenumber = true

-- Set highlight on search
vim.o.hlsearch = true

-- Customize terminal's title
-- vim.o.title = true
-- vim.o.titlestring = [[%f - nvim]]

-- Show mode in command line
vim.o.showmode = true

-- Max autocomplete height
vim.o.pumheight = 15

-- System clipboard
vim.o.clipboard = 'unnamedplus'

-- Enable mouse mode
vim.o.mouse = 'a'

-- Enable break indent
vim.o.breakindent = true

-- Save undo history
vim.o.undofile = true

-- No swapfile
vim.o.swapfile = false

-- Case insensitive searching UNLESS /C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

-- Set colorscheme
vim.o.termguicolors = true

-- hide search anzu
vim.cmd [[set shortmess +=S]]

-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menuone,noselect'

-- Use spaces instead of tabs
vim.o.expandtab = true
vim.o.smarttab = true

-- Default tab width is 2
vim.o.tabstop = 2
vim.o.shiftwidth = 2
-- vim.bo.softtabstop = 2
vim.o.smartindent = true

-- Preview incremental substitute
vim.o.inccommand = true

-- Make more sense spliting
vim.o.splitbelow = true -- Put new windows below current
vim.o.splitright = true -- Put new windows right of current

-- Only one statusline
vim.o.laststatus = 3

-- Show lists
vim.o.list = true
vim.o.listchars = 'tab:│ '

vim.o.scrolloff = 4

vim.o.previewheight = 17

-- Discontinue comments on new line
vim.api.nvim_create_autocmd('BufEnter', {
   callback = function()
      vim.opt.formatoptions = vim.opt.formatoptions - { 'c', 'r', 'o' }
   end,
})

-- lua tabstop
vim.api.nvim_create_autocmd('FileType', {
   pattern = 'lua',
   command = 'setlocal shiftwidth=3 tabstop=3',
})

-- go tabstop
vim.api.nvim_create_autocmd('FileType', {
   pattern = 'go',
   command = 'setlocal shiftwidth=4 tabstop=4',
})

local function augroup(name)
   return vim.api.nvim_create_augroup(name, { clear = true })
end

-- Highlight on yank
vim.api.nvim_create_autocmd('TextYankPost', {
   group = augroup 'HighlightYank',
   callback = function()
      vim.highlight.on_yank { timeout = 100 }
   end,
})

-- close some filetypes with <q>
vim.api.nvim_create_autocmd('FileType', {
   group = augroup 'CloseWithQ',
   pattern = {
      'qf',
      'help',
      'man',
      'notify',
      'lspinfo',
      'startuptime',
      'tsplayground',
      'git',
      'fugitive',
   },
   callback = function(event)
      vim.bo[event.buf].buflisted = false
      vim.keymap.set('n', 'q', '<cmd>close<cr>', { buffer = event.buf, silent = true })
   end,
})

-- reload after change tmux config
vim.api.nvim_create_autocmd('BufWritePost', {
   pattern = { '*tmux.conf' },
   command = "execute 'silent !tmux source <afile> --silent'",
})

vim.api.nvim_create_user_command('YankFilePath', function()
   local path = vim.fn.expand '%:p'
   vim.fn.setreg('+', path)
   vim.notify('Copied "' .. path .. '" to the clipboard!')
end, {})
