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
      d = { ':bp|bd#<CR>', 'Delete buffer' },
      n = { ':bn<CR>', 'Next buffer' },
      p = { ':bp<CR>', 'Previous buffer' },
      l = { ':e #<CR>', 'Last buffer' },
      b = { '<cmd>Telescope buffers sort_lastused=true ignore_current_buffer=true<CR>', 'Switch to buffer' },
   },

   f = {
      name = 'File',
      f = { '<cmd>Telescope find_files<CR>', 'Find files' },
      -- f = { '<cmd>Telescope find_files cwd=%:p:h<CR>', 'Find current dir files' },
      r = { '<cmd>Telescope oldfiles<CR>', 'Recent files' },
      d = { '<cmd>Oil<CR>', 'File Explorer' },
      G = { ':e ~/code/exp/play/main.go<CR>', 'Goplay' },
      i = { ':e ~/.config/nvim/init.lua<CR>', 'Config nvim' },
      t = { ':e ~/.config/alacritty/alacritty.yml<CR>', 'Config terminal' },
      -- t = { ':e ~/.config/kitty/kitty.conf<CR>' },
      x = { ':e ~/.config/tmux/tmux.conf<CR>', 'Config tmux' },
      y = { '<cmd>YankFilePath<CR>', 'copy file path to clipboard' },
      z = { ':e ~/.zshrc<CR>', '.zshrc' },
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
      k = { '<cmd>GrapplePopup tags<CR>', 'Tag popup' },
      i = { '<cmd>GrappleTag<CR>', 'Tag file' },
      u = { '<cmd>GrappleUntag<CR>', 'Untag file' },
      n = { '<cmd>GrappleCycle forward<CR>', 'Switch to next tag' },
      p = { '<cmd>GrappleCycle backward<CR>', 'Switch to prev tag' },
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
      h = { '<cmd>Telescope help_tags<CR>', 'Help tags' },
      k = { '<cmd>Telescope keymaps<CR>', 'Keymaps' },
      l = { '<cmd>Telescope highlights<CR>', 'Highlights' },
   },

   c = {
      name = 'Code',
      m = { '<cmd>Mason<CR>', 'Mason' },
      x = { '<cmd>TroubleToggle<CR>', 'List errors' },
      R = { '<cmd>Telescope lsp_references<CR>', 'References' },
      s = { '<cmd>Telescope lsp_document_symbols<CR>', 'Symbols' },
      a = { vim.lsp.buf.code_action, 'Code action' },
      r = { vim.lsp.buf.rename, 'Rename' },
   },

   s = {
      name = 'Search',
      d = { '<cmd>Telescope dir live_grep<CR>', 'Grep in directory' },
      p = { '<cmd>Telescope live_grep<CR>', 'Grep in project' },
      l = { '<cmd>Telescope current_buffer_fuzzy_find<CR>', 'Search this buffer' },
   },

   t = {
      name = 'Test',
      f = { '<cmd>TestFile<CR>', 'Test file' },
      a = { '<cmd>TestSuite<CR>', 'Test suite' },
      s = { '<cmd>TestNearest<CR>', 'Test single' },
      t = { '<cmd>TestLast<CR>', 'Test last' },
      b = { '<cmd>TestVisit<CR>', 'Test visit' },
   },

   g = {
      name = 'Git',
      [']'] = { '<cmd>Gitsigns next_hunk<CR>', 'Next hunk' },
      ['['] = { '<cmd>Gitsigns prev_hunk<CR>', 'Prev hunk' },
      h = { '<cmd>Gitsigns preview_hunk<CR>', 'Preview hunk' },
      s = { '<cmd>Gitsigns stage_hunk<CR>', 'Stage hunk' },
      u = { '<cmd>Gitsigns undo_stage_hunk<CR>', 'Unstage hunk' },
      x = { '<cmd>Gitsigns reset_stage_hunk<CR>', 'Reset hunk' },
      d = { '<cmd>DiffviewOpen<CR>', 'Diffview' },
      g = { ':tab Git<CR>', 'Git' },
      c = { ':Git commit<CR>', 'Commit' },
      b = { ':Git branch<CR>', 'Branch' },
      l = { ':Git log<CR>', 'Log' },
      B = { ':Git blame<CR>', 'Blame' },
      F = { ':Git pull --rebase<CR>', 'Pull' },
      P = { ':Git push<CR>', 'Push' },
   },

   ['.'] = { '<cmd>Telescope resume<CR>', 'Resume' },
   [':'] = { '<cmd>Telescope command_history<CR>', 'Command history' },
   ['<space>'] = { '<cmd>Telescope commands<CR>', 'Commands' },
}, { prefix = '<leader>', mode = 'n' })

wk.register {
   K = { lsputil.hover_doc, 'Hover documentation' },
   g = {
      d = { vim.lsp.buf.definition, 'Goto definition' },
      I = { vim.lsp.buf.implementation, 'Goto implementation' },
   },
   [']e'] = { vim.diagnostic.goto_prev, 'Next diagnostic' },
   ['[e'] = { vim.diagnostic.goto_prev, 'Previous diagnostic' },
}
