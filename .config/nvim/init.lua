-- harpoon
-- gitmux config + new icons
-- cmp sort comparator

-- reuse only one hover split

-- fugitive highlight
-- fugitive remap
-- fugitive fullscreen

-- diffview stage/unstage?

-- git alias completion?
-- docker completions?

-- toggle line numbers

-- fzf open new tmux tab with selected directory/project roots
-- tmux close pane without confirm

-- dwm + patches
-- xorg keyboard shortcut/mouse
-- xorg hidpi
-- bar wifi,battery,volume,bluetooth
-- tray fcitx,zoom
-- feh
-- bluetooth blueberry
-- volume pavucontrol
-- wifi networkmanager
-- shutdown menu
-- lockscreen
-- login screen
-- eww menu?
-- default applications?

-- partition
-- endeavouros lts kernel?

-- Later
-- elixir
-- ocaml
-- tmuxp

-- Maybe never
-- tmux fzf launch app in new tab (top,k9s)
-- dashboard/ project management
-- dap
-- haskell
-- clojure
-- parinfer
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
