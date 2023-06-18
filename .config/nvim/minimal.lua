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

require('lazy').setup {
   {
      'windwp/nvim-autopairs',
      config = function()
         require('nvim-autopairs').setup {}
      end,
   },
}

require 'config.mappings'
