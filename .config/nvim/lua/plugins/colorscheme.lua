return {
   {
      'catppuccin/nvim',
      enabled = true,
      priority = 1000,
      name = 'catppuccin',
      opts = {
         flavour = 'mocha',
         transparent_background = true,
         highlight_overrides = {
            mocha = function(mocha)
               return {
                  Pmenu = { bg = mocha.mantle },
               }
            end,
         },
         integrations = {
            fidget = true,
            gitsigns = true,
            mason = true,
            cmp = true,
            native_lsp = {
               enabled = true,
            },
            treesitter = true,
            -- overseer = true,
            -- telescope = true,
            -- lsp_trouble = true,
            which_key = true,
         },
      },
   },
}
