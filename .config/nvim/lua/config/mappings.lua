local map = require('utils').map

-- [[ Basic Keymaps ]]
map('', ';', ':')
-- map('c', 'kj', '<esc>')

-- Keymaps for better default experience
map({ 'n', 'v' }, '<Space>', '<Nop>')

-- Remap for dealing with word wrap
map('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true })
map('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true })

-- Better indents
map('v', '>', '>gv')
map('v', '<', '<gv')

-- Buffers
map('n', '<leader>bd', ':bd<cr>')
map('n', '<leader>bn', ':bn<cr>')
map('n', '<leader>bp', ':bp<cr>')
map('n', '<leader>bl', ':e #<cr>')

-- Files
map('n', '<leader>fi', ':e ~/.config/nvim/init.lua<cr>')
map('n', '<leader>fz', ':e ~/.zshrc<cr>')
map('n', '<leader>fx', ':e ~/.config/tmux/tmux.conf<cr>')
map('n', '<leader>ft', ':e ~/.config/alacritty/alacritty.yaml<cr>')
-- map('n', '<leader>ft', ':e ~/.config/kitty/kitty.conf<cr>')
map('n', '<leader>fG', ':e ~/code/exp/play/main.go<cr>')

-- Windows
map('n', '<leader>wo', '<C-W>o')
map('n', '<leader>wd', '<C-W>c')
map('n', '<leader>wh', '<C-W>h')
map('n', '<leader>wj', '<C-W>j')
map('n', '<leader>wk', '<C-W>k')
map('n', '<leader>wl', '<C-W>l')
map('n', '<leader>wv', '<C-W>v')
map('n', '<leader>ws', '<C-W>s')
