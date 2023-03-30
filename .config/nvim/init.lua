-- check lsp setting with kickstart

-- zoxide/z.lua & fzf
-- tmux popup-fzf select directory new tab

-- tmux popup lazygit?
--- save serverfile in cwd
--- read current directory nvim server file
--- open popup start NVIM_SERVER=blahblah lazygit
--- edit send file to nvimserver & quit tmux popup

-- tmux popup-fzf run executables in tmux window

-- diffview

-- Maybe later later...
-- rest
-- elixir
-- clojure
-- parinfer
-- ocaml
-- haskell
-- tmuxp
-- github pr
-- dap
-- cmp sort comparator

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

require('lazy').setup('plugins', {
   defaults = {
      lazy = true,
   },
   ui = {
      border = 'rounded',
   },
   change_detection = {
      notify = false,
   },
})

require 'config.mappings'

-- vim.env.GIT_EDITOR = 'nvr --servername ' .. vim.v.servername .. " -cc vsplit --remote-wait +'set bufhidden=delete'"
vim.cmd [[set statusline=%!v:lua.require'statusline'.output()]]
vim.cmd 'colorscheme catppuccin'

-- vim: ts=3 sts=3 sw=3 et
