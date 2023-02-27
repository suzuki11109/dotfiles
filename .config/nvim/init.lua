-- git diff side by side
-- kitty tab length
-- lazygit/ neovim remote

-- bookmark/grapple/harpoon/jump mark
-- ruby rails

-- Later
-- octo
-- clojure
-- parinfer
-- angular
-- elixir
-- ocaml
-- haskell

-- Maybe never
-- rest
-- resession
-- overseer
-- fzf find app open new tmux window
-- runner to tmux
-- new session from shell
-- new tmux session neovim editor window
--

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

vim.cmd [[colorscheme catppuccin]]

-- vim: ts=3 sts=3 sw=3 et
