-- buffers show hidden file
-- neotest
-- yaml k8s support

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

if os.getenv 'NVIM' ~= nil then
   require('lazy').setup {
      { 'willothy/flatten.nvim', config = true },
   }
   return
end

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

vim.cmd 'colorscheme catppuccin'

-- vim: ts=3 sts=3 sw=3 et
