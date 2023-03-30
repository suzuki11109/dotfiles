local map = require('utils').map
local lsputil = require 'plugins.lsp.util'

-- [[ Basic Keymaps ]]
map('', ';', ':')

-- Keymaps for better default experience
map({ 'n' }, '<Space>', '<Nop>')

-- Remap for dealing with word wrap
map('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true })
map('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true })

-- Better indents
map('v', '>', '>gv')
map('v', '<', '<gv')

local wk = require 'which-key'

wk.register({
   b = {
      name = 'Buffer',
      d = { ':bp|bd#<cr>', 'Delete buffer' },
      n = { ':bn<cr>', 'Next buffer' },
      p = { ':bp<cr>', 'Previous buffer' },
      l = { ':e #<cr>', 'Last buffer' },
      b = { '<cmd>Telescope buffers sort_lastused=true ignore_current_buffer=true<cr>', 'Switch to buffer' },
   },

   f = {
      name = 'File',
      f = { '<cmd>Telescope find_files<cr>', 'Find files' },
      -- f = { '<cmd>Telescope find_files cwd=%:p:h<cr>', 'Find current dir files' },
      r = { '<cmd>Telescope oldfiles<cr>', 'Recent files' },
      d = { '<cmd>Oil<cr>', 'File Explorer' },
      G = { ':e ~/code/exp/play/main.go<cr>', 'Goplay' },
      i = { ':e ~/.config/nvim/init.lua<cr>', 'Config nvim' },
      t = { ':e ~/.config/alacritty/alacritty.yml<cr>', 'Config terminal' },
      -- t = { ':e ~/.config/kitty/kitty.conf<cr>' },
      x = { ':e ~/.config/tmux/tmux.conf<cr>', 'Config tmux' },
      z = { ':e ~/.zshrc<cr>', '.zshrc' },
   },

   w = {
      name = 'Window',
      o = { '<C-W>o', 'Close other window' },
      d = { '<C-W>c', 'Close this window' },
      h = { '<C-W>h', 'Left window' },
      j = { '<C-W>j', 'Below window' },
      k = { '<C-W>k', 'Above window' },
      l = { '<C-W>l', 'Right window' },
      v = { '<C-W>v', 'Split vertical' },
      s = { '<C-W>s', 'Split horizontal' },
   },

   k = {
      name = 'Mark',
      k = { '<cmd>GrapplePopup tags<cr>', 'Tag popup' },
      i = { '<cmd>GrappleTag<cr>', 'Tag file' },
      u = { '<cmd>GrappleUntag<cr>', 'Untag file' },
      n = { '<cmd>GrappleCycle forward<cr>', 'Switch to next tag' },
      p = { '<cmd>GrappleCycle backward<cr>', 'Switch to prev tag' },
      ['1'] = { '<cmd>GrappleSelect key=1<CR>', 'Switch to tag 1' },
      ['2'] = { '<cmd>GrappleSelect key=2<CR>', 'Switch to tag 2' },
      ['3'] = { '<cmd>GrappleSelect key=3<CR>', 'Switch to tag 3' },
      ['4'] = { '<cmd>GrappleSelect key=4<CR>', 'Switch to tag 4' },
      ['5'] = { '<cmd>GrappleSelect key=5<CR>', 'Switch to tag 5' },
      ['6'] = { '<cmd>GrappleSelect key=6<CR>', 'Switch to tag 6' },
      ['7'] = { '<cmd>GrappleSelect key=7<CR>', 'Switch to tag 7' },
      ['8'] = { '<cmd>GrappleSelect key=8<CR>', 'Switch to tag 8' },
      ['9'] = { '<cmd>GrappleSelect key=9<CR>', 'Switch to tag 9' },
      ['0'] = { '<cmd>GrappleSelect key=10<CR>', 'Swtich to tag 10' },
   },

   h = {
      name = 'Help',
      h = { '<cmd>Telescope help_tags<cr>', 'Help tags' },
      k = { '<cmd>Telescope keymaps<cr>', 'Keymaps' },
      l = { '<cmd>Telescope highlights<cr>', 'Highlights' },
   },

   c = {
      name = 'Code',
      m = { '<cmd>Mason<cr>', 'Mason' },
      x = { '<cmd>TroubleToggle<cr>', 'List errors' },
      R = { '<cmd>Telescope lsp_references<cr>', 'References' },
      s = { '<cmd>Telescope lsp_document_symbols<cr>', 'Symbols' },
      a = { vim.lsp.buf.code_action, 'Code action' },
      r = { vim.lsp.buf.rename, 'Rename' },
   },

   s = {
      name = 'Search',
      d = { '<cmd>Telescope dir live_grep<CR>', 'Grep in directory' },
      p = { '<cmd>Telescope live_grep<cr>', 'Grep in project' },
      l = { '<cmd>Telescope current_buffer_fuzzy_find<cr>', 'Search this buffer' },
   },

   t = {
      name = 'Test',
      f = { '<cmd>TestFile<cr>', 'Test file' },
      a = { '<cmd>TestSuite<cr>', 'Test suite' },
      s = { '<cmd>TestNearest<cr>', 'Test single' },
      t = { '<cmd>TestLast<cr>', 'Test last' },
      b = { '<cmd>TestVisit<cr>', 'Test visit' },
   },

   g = {
      name = 'Git',
      [']'] = { '<cmd>Gitsigns next_hunk<cr>', 'Next hunk' },
      ['['] = { '<cmd>Gitsigns prev_hunk<cr>', 'Prev hunk' },
      h = { '<cmd>Gitsigns preview_hunk<cr>', 'Preview hunk' },
      s = { '<cmd>Gitsigns stage_hunk<cr>', 'Stage hunk' },
      u = { '<cmd>Gitsigns undo_stage_hunk<cr>', 'Unstage hunk' },
      x = { '<cmd>Gitsigns reset_stage_hunk<cr>', 'Reset hunk' },
   },

   ['.'] = { '<cmd>Telescope resume<cr>', 'Resume' },
   [':'] = { '<cmd>Telescope command_history<cr>', 'Command history' },
   ['<space>'] = { '<cmd>Telescope commands<cr>', 'Commands' },
}, { prefix = '<leader>', mode = 'n' })

wk.register {
   s = { '<cmd>HopChar2<cr>', 'hop' },
   K = { lsputil.hover_doc, 'Hover documentation' },
   g = {
      d = { vim.lsp.buf.definition, 'Goto definition' },
      I = { vim.lsp.buf.implementation, 'Goto implementation' },
   },
   [']e'] = { vim.diagnostic.goto_prev, 'Next diagnostic' },
   ['[e'] = { vim.diagnostic.goto_prev, 'Previous diagnostic' },
}
