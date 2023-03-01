-- change font
-- tmux new tab with name prompt
-- new tmux session neovim editor window
-- tmux manager runner
-- tmux remove sensible
-- bigger toggleterm
-- next toggleterm
-- null-ls

-- project root nvim
-- tmux xdgconfig

-- dashboard

-- telescope search under directory

-- clojure
-- parinfer
-- angular

-- bookmark/grapple/harpoon/jump mark

-- elixir
-- ocaml
-- haskell

-- Maybe never
-- fzf find app open new tmux window
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

vim.env.GIT_EDITOR = 'nvr --servername ' .. vim.v.servername .. " -cc vsplit --remote-wait +'set bufhidden=delete'"
vim.cmd [[set statusline=%!v:lua.require'statusline'.output()]]
vim.cmd [[colorscheme catppuccin]]

-- vim: ts=3 sts=3 sw=3 et
