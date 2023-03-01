return {
   {
      'catppuccin/nvim',
      name = 'catppuccin',
      opts = {
         flavour = 'mocha',
         transparent_background = true,
         highlight_overrides = {
            mocha = function(mocha)
               return {
                  Pmenu = { bg = mocha.base },
               }
            end,
         },
         integrations = {
            cmp = true,
            fidget = true,
            gitsigns = true,
            hop = true,
            mason = true,
            nvimtree = true,
            treesitter = true,
            telescope = true,
            lsp_trouble = true,
            native_lsp = {
               enabled = true,
            },
         },
      },
   },
}
