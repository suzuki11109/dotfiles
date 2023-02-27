return {
   {
      'catppuccin/nvim',
      name = 'catppuccin',
      opts = {
         flavour = 'mocha',
         transparent_background = true,
         integrations = {
            fidget = true,
            mason = true,
            gitsigns = true,
            hop = true,
            cmp = true,
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
