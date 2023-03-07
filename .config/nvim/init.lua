-- hoppscoth api tester
-- dbeaver client vi

-- Later
-- harpoon
-- elixir
-- ocaml
-- tmuxp

-- Maybe never
-- tmux fzf launch app in new tab (top,k9s)
-- tmux popup
-- dashboard/ project management
-- dap
-- haskell
-- clojure
-- parinfer
-- fzf find executable open new tmux window
-- resession
-- overseer
-- octo
-- rest

local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
   vim.fn.system {
      'git',
      'clone',
      '--filter=blob:none',
      'https://github.com/folke/lazy.nvim.git',
      '--branch=stable',
      lazypath,
   }
end

vim.opt.rtp:prepend(lazypath)

require 'config.options'
require 'config.mappings'

require('lazy').setup('plugins', {
   ui = {
      border = 'rounded',
   },
   change_detection = {
      notify = false,
   },
})

-- vim.env.GIT_EDITOR = 'nvr --servername ' .. vim.v.servername .. " -cc vsplit --remote-wait +'set bufhidden=delete'"
vim.cmd [[set statusline=%!v:lua.require'statusline'.output()]]
vim.cmd [[colorscheme catppuccin]]

-- vim: ts=3 sts=3 sw=3 et
