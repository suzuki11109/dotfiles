local map = require('utils').map
-- local lsputil = require 'plugins.lsp.util'

-- [[ Basic Keymaps ]]
map('', ';', ':')

-- Keymaps for better default experience
map({ 'n' }, '<Space>', '<Nop>')

-- Remap for dealing with word wrap
map('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true })
map('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true })

map('n', '<C-h>', '<C-w>h')
map('n', '<C-j>', '<C-w>j')
map('n', '<C-k>', '<C-w>k')
map('n', '<C-l>', '<C-w>l')

-- Better indents
map('v', '>', '>gv')
map('v', '<', '<gv')

local wk = require 'which-key'

wk.register({
   ['<space>'] = { '<cmd>Telescope commands<CR>', 'Commands' },
   ['.'] = { '<cmd>Telescope resume<CR>', 'Resume' },
   [':'] = { '<cmd>Telescope command_history<CR>', 'Command history' },

   b = {
      name = 'Buffer',
      b = { '<cmd>Telescope buffers sort_lastused=true ignore_current_buffer=true<CR>', 'Switch to buffer' },
      d = { ':bp|bd#<CR>', 'Close buffer' },
      l = { ':e #<CR>', 'Last buffer' },
      n = { ':bn<CR>', 'Next buffer' },
      p = { ':bp<CR>', 'Previous buffer' },
   },

   c = {
      name = 'Code',
      a = { vim.lsp.buf.code_action, 'Code action' },
      d = { vim.lsp.buf.definition, 'Goto definition' },
      m = { '<cmd>Mason<CR>', 'Mason' },
      r = { vim.lsp.buf.rename, 'Rename' },
      R = { '<cmd>Telescope lsp_references<CR>', 'References' },
      s = { '<cmd>Telescope lsp_document_symbols<CR>', 'Symbols' },
      x = { '<cmd>TroubleToggle<CR>', 'List errors' },
   },

   f = {
      name = 'File',
      d = { '<cmd>Oil<CR>', 'File Explorer' },
      f = { '<cmd>Telescope find_files<CR>', 'Find files' },
      F = { '<cmd>Telescope find_files cwd=%:p:h<CR>', 'Find current dir files' },
      G = { ':e ~/code/exp/play/main.go<CR>', 'Goplay' },
      i = { ':e ~/.config/nvim/init.lua<CR>', 'Config nvim' },
      r = { '<cmd>Telescope oldfiles<CR>', 'Recent files' },
      T = { ':e ~/.config/alacritty/alacritty.yml<CR>', 'Config terminal' },
      t = { ':e ~/.config/kitty/kitty.conf<CR>', 'Config kitty' },
      x = { ':e ~/.config/tmux/tmux.conf<CR>', 'Config tmux' },
      y = { '<cmd>YankFilePath<CR>', 'copy file path to clipboard' },
      z = { ':e ~/.zshrc<CR>', '.zshrc' },
   },

   h = {
      name = 'Help',
      h = { '<cmd>Telescope help_tags<CR>', 'Help tags' },
      k = { '<cmd>Telescope keymaps<CR>', 'Keymaps' },
      l = { '<cmd>Telescope highlights<CR>', 'Highlights' },
      t = { '<cmd>Telescope colorscheme<CR>', 'Colorscheme' },
   },

   k = {
      name = 'Mark',
      i = { '<cmd>GrappleTag<CR>', 'Tag file' },
      k = { '<cmd>GrapplePopup tags<CR>', 'Tag popup' },
      n = { '<cmd>GrappleCycle forward<CR>', 'Switch to next tag' },
      p = { '<cmd>GrappleCycle backward<CR>', 'Switch to prev tag' },
      u = { '<cmd>GrappleUntag<CR>', 'Untag file' },
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

   m = {
      name = 'Terminal',
      m = { '<cmd>TermSelect<CR>', 'Toggle terminal' },
      ['1'] = { '<cmd>1ToggleTerm<CR>', 'Switch to term 1' },
      ['2'] = { '<cmd>2ToggleTerm<CR>', 'Switch to term 2' },
      ['3'] = { '<cmd>3ToggleTerm<CR>', 'Switch to term 3' },
      ['4'] = { '<cmd>4ToggleTerm<CR>', 'Switch to term 4' },
      ['5'] = { '<cmd>5ToggleTerm<CR>', 'Switch to term 5' },
   },

   n = {
      name = 'Run',
      o = { '<cmd>OverseerToggle<CR>', 'Toggle task window' },
      n = { '<cmd>OverseerRun<CR>', 'Run tasks from template' },
      c = { '<cmd>OverseerRunCmd<CR>', 'Run cmd in shell' },
   },

   r = {
      name = 'Rest',
      r = { '<Plug>RestNvim', 'Run request at point' },
      p = { '<Plug>RestNvimPreview', 'Preview request command' },
      l = { '<Plug>RestNvimLast', 'Rerun last request' },
   },

   s = {
      name = 'Search',
      d = { '<cmd>Telescope dir live_grep<CR>', 'Grep in directory' },
      l = { '<cmd>Telescope current_buffer_fuzzy_find<CR>', 'Search this buffer' },
      p = { '<cmd>Telescope live_grep<CR>', 'Grep in project' },
   },

   t = {
      name = 'Test',
      a = { '<cmd>TestSuite<CR>', 'Test suite' },
      b = { '<cmd>TestVisit<CR>', 'Test visit' },
      f = { '<cmd>TestFile<CR>', 'Test file' },
      s = { '<cmd>TestNearest<CR>', 'Test single' },
      t = { '<cmd>TestLast<CR>', 'Test last' },
   },

   w = {
      name = 'Window',
      c = { '<C-W>c', 'Close this window' },
      h = { '<C-W>h', 'Left window' },
      j = { '<C-W>j', 'Below window' },
      k = { '<C-W>k', 'Above window' },
      l = { '<C-W>l', 'Right window' },
      o = { '<C-W>o', 'Close other window' },
      s = { '<C-W>s', 'Split horizontal' },
      v = { '<C-W>v', 'Split vertical' },
      z = { '<cmd>$quit<CR>', 'Close last window' },
   },

   g = {
      name = 'Git',
      --    b = { ':Git branch<CR>', 'Branch' },
      h = { '<cmd>Gitsigns preview_hunk<CR>', 'Preview hunk' },
      -- s = { '<cmd>Gitsigns stage_hunk<CR>', 'Stage hunk' },
      -- u = { '<cmd>Gitsigns undo_stage_hunk<CR>', 'Unstage hunk' },
      -- x = { '<cmd>Gitsigns reset_stage_hunk<CR>', 'Reset hunk' },
      --    g = { ':tab Git<CR>', 'Git' },
      --    c = { ':Git commit<CR>', 'Commit' },
      --    l = { ':Git log<CR>', 'Log' },
      --    B = { ':Git blame<CR>', 'Blame' },
      --    F = { ':Git pull --rebase<CR>', 'Pull' },
      --    P = { ':Git push<CR>', 'Push' },
      [']'] = { '<cmd>Gitsigns next_hunk<CR>', 'Next hunk' },
      ['['] = { '<cmd>Gitsigns prev_hunk<CR>', 'Prev hunk' },
   },
}, { prefix = '<leader>', mode = 'n' })

wk.register {
   K = { '<cmd>DocsViewUpdate<CR>', 'View docs' },
   g = {
      d = { vim.lsp.buf.definition, 'Goto definition' },
      I = { vim.lsp.buf.implementation, 'Goto implementation' },
   },
   [']e'] = {
      function()
         vim.diagnostic.goto_prev { float = false }
      end,
      'Next diagnostic',
   },
   ['[e'] = {
      function()
         vim.diagnostic.goto_prev { float = false }
      end,
      'Previous diagnostic',
   },
}

vim.api.nvim_create_autocmd('FileType', {
   pattern = { 'typescript', 'typescriptreact' },
   callback = function()
      wk.register {
         ['gd'] = { '<cmd>TypescriptGoToSourceDefinition<CR>', 'Goto source definition' },
      }
   end,
})
