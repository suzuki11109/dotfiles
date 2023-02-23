-- bookmark/grapple/harpoon/jump mark
-- refactor module

-- kitty scroll vi mode
-- fzf git add select
-- markdown preview glow

-- git diffview

-- my go playgournd
-- lsp  statusline

-- ruby rails
-- angular
-- clojure
-- parinfer
-- elixir
-- haskell
-- ocaml
-- rest

-- code runner
-- resession
-- overseer
-- fzf find app open new tmux window
-- runner to tmux
-- new session from shell
-- new tmux session neovim editor window
--
-- Set <space> as the leader key
-- See `:help mapleader`
--  NOTE: Must happen before plugins are required (otherwise wrong leader will be used)
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- Decrease update time
vim.o.updatetime = 250
vim.wo.signcolumn = 'yes'

-- Make line numbers default
vim.wo.number = true
vim.o.relativenumber = true

local function map(mode, keys, func, opts)
   local options = { noremap = true, silent = true }
   if opts then
      options = vim.tbl_extend('force', options, opts)
   end

   vim.keymap.set(mode, keys, func, opts)
end

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

local lsp_format = function(bufnr)
   vim.lsp.buf.format {
      filter = function(client)
         -- always use null-ls only
         return client.name == 'null-ls'
      end,
      bufnr = bufnr,
   }
end

local hover_doc = function()
   vim.lsp.buf_request(0, 'textDocument/hover', vim.lsp.util.make_position_params(), function(_, result, ctx, config)
      config = config or {}
      config.focus_id = ctx.method
      if not (result and result.contents) then
         vim.notify 'No information available'
         return
      end
      local markdown_lines = vim.lsp.util.convert_input_to_markdown_lines(result.contents)
      markdown_lines = vim.lsp.util.trim_empty_lines(markdown_lines)
      if vim.tbl_isempty(markdown_lines) then
         vim.notify 'No information available'
         return
      end

      vim.api.nvim_command [[ new ]]
      vim.api.nvim_command [[ resize 15 ]]
      vim.api.nvim_buf_set_lines(0, 0, 1, false, markdown_lines)
      vim.api.nvim_command [[ setlocal ft=markdown ]]
      vim.api.nvim_command [[ setlocal norelativenumber ]]
      vim.api.nvim_command [[ setlocal nonumber ]]
      vim.api.nvim_command [[ nnoremap <buffer>q <C-W>c ]]
      vim.api.nvim_command [[ setlocal buftype+=nofile ]]
      vim.api.nvim_command [[ setlocal nobl ]]
      vim.api.nvim_command [[ setlocal conceallevel=2 ]]
      vim.api.nvim_command [[ setlocal concealcursor+=n ]]
   end)
end

local lsp_format_augroup = vim.api.nvim_create_augroup('LspFormat', {})

-- local has_words_before = function()
--    unpack = unpack or table.unpack
--    local line, col = unpack(vim.api.nvim_win_get_cursor(0))
--    return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match '%s' == nil
-- end

local function metals_status_handler(_, status, ctx)
   local val = {}
   if status.hide then
      val = { kind = 'end' }
   elseif status.show then
      val = { kind = 'begin', message = status.text }
   elseif status.text then
      val = { kind = 'report', message = status.text }
   else
      return
   end
   local info = { client_id = ctx.client_id }
   local msg = { token = 'metals', value = val }
   vim.lsp.handlers['$/progress'](nil, msg, info)
end

local function goimports(wait_ms)
   local params = vim.lsp.util.make_range_params()
   params.context = { only = { 'source.organizeImports' } }

   local result = vim.lsp.buf_request_sync(0, 'textDocument/codeAction', params, wait_ms)
   for _, res in pairs(result or {}) do
      for _, r in pairs(res.result or {}) do
         if r.edit then
            vim.lsp.util.apply_workspace_edit(r.edit, vim.lsp.util._get_offset_encoding())
         else
            vim.lsp.buf.execute_command(r.command)
         end
      end
   end
end

--  This function gets run when an LSP connects to a particular buffer.
local on_attach = function(client, bufnr)
   -- Diagnostic keymaps
   map('n', '[e', function()
      vim.diagnostic.goto_prev { float = false }
   end, { desc = 'previous diagnostic' })
   map('n', ']e', function()
      vim.diagnostic.goto_next { float = false }
   end, { desc = 'next diagnostic' })
   map({ 'n', 'v' }, '<leader>cr', vim.lsp.buf.rename, { desc = 'rename' })
   map('n', '<leader>ca', vim.lsp.buf.code_action, { desc = 'code action' })
   map({ 'n', 'v' }, 'gd', vim.lsp.buf.definition, { desc = 'goto definition' })
   map({ 'n', 'v' }, 'gI', vim.lsp.buf.implementation, { desc = 'goto implementation' })
   -- telescope helper
   map({ 'n', 'v' }, 'gr', require('telescope.builtin').lsp_references, { desc = 'find references' })
   map('n', '<leader>cs', require('telescope.builtin').lsp_document_symbols, { desc = 'document symbols' })

   -- map({ 'n', 'v' }, 'K', vim.lsp.buf.hover, { desc = 'Hover Documentation' })
   map({ 'n', 'v' }, 'K', hover_doc, { desc = 'Hover Documentation' })

   vim.diagnostic.config {
      virtual_text = false,
   }

   -- vim.lsp.handlers['textDocument/hover'] = vim.lsp.with(vim.lsp.handlers.hover, {
   --    border = 'rounded',
   -- })

   -- Create a command `:Format` local to the LSP buffer
   vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
      lsp_format(bufnr)
   end, { desc = 'format current buffer' })

   local signs = { Error = ' ', Warn = ' ', Hint = ' ', Info = ' ' }
   for type, icon in pairs(signs) do
      local hl = 'DiagnosticSign' .. type
      vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
   end

   if client.server_capabilities.documentFormattingProvider then
      vim.api.nvim_clear_autocmds { group = lsp_format_augroup, buffer = bufnr }
      vim.api.nvim_create_autocmd('BufWritePre', {
         group = lsp_format_augroup,
         buffer = bufnr,
         callback = function()
            lsp_format(bufnr)
         end,
      })
   end

   vim.api.nvim_create_autocmd('CursorHold', {
      buffer = bufnr,
      callback = function()
         -- don't show diagnostic if there is already hover doc, but it conflict with doc
         -- for _, winid in pairs(vim.api.nvim_tabpage_list_wins(0)) do
         --    if vim.api.nvim_win_get_config(winid).zindex then
         --       return
         --    end
         -- end
         local opts = {
            focusable = false,
            close_events = { 'BufLeave', 'CursorMoved', 'InsertEnter', 'FocusLost' },
            border = 'rounded',
            source = 'always',
            prefix = ' ',
            scope = 'line', -- line or cursor
         }
         vim.diagnostic.open_float(nil, opts)
      end,
   })
end

require('lazy').setup({
   { -- LSP Configuration & Plugins
      'neovim/nvim-lspconfig',
      event = { 'BufReadPre', 'BufNewFile' },
      dependencies = {
         { 'folke/neoconf.nvim', cmd = 'Neoconf', config = true },
         { 'folke/neodev.nvim', opts = { experimental = { pathStrict = true } } },
         'hrsh7th/cmp-nvim-lsp',
         'mason.nvim',
         'williamboman/mason-lspconfig.nvim',
         'fidget.nvim',
      },
      opts = {
         servers = {
            jsonls = {},
            gopls = {},
            tsserver = {},
            pyright = {},
            lua_ls = {
               settings = {
                  Lua = {
                     workspace = {
                        checkThirdParty = false,
                     },
                     completion = {
                        callSnippet = 'Replace',
                     },
                  },
               },
            },
         },
      },
      config = function(_, opts)
         require('mason-lspconfig').setup()
         require('lspconfig.ui.windows').default_options.border = 'rounded'

         local servers = opts.servers
         local capabilities = vim.lsp.protocol.make_client_capabilities()
         capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

         local lspconfig = require 'lspconfig'
         for server, server_opts in pairs(servers) do
            local servopts = vim.tbl_deep_extend('force', {
               capabilities = vim.deepcopy(capabilities),
               on_attach = on_attach,
            }, server_opts or {})
            lspconfig[server].setup(servopts)
         end
      end,
   },

   {
      'williamboman/mason.nvim',
      cmd = 'Mason',
      keys = { { '<leader>cm', '<cmd>Mason<cr>', desc = 'Mason' } },
      config = function()
         require('mason').setup {
            ui = {
               border = 'rounded',
            },
         }
      end,
   },

   {
      'jose-elias-alvarez/null-ls.nvim',
      event = { 'BufReadPre', 'BufNewFile' },
      dependencies = { 'mason.nvim' },
      opts = function()
         local nls = require 'null-ls'
         return {
            sources = {
               nls.builtins.formatting.prettierd,
               nls.builtins.formatting.stylua,
               nls.builtins.formatting.gofmt,
            },
         }
      end,
   },

   { -- Useful status updates for LSP
      'j-hui/fidget.nvim',
      config = function()
         require('fidget').setup {
            window = {
               blend = 0,
            },
         }
      end,
   },

   {
      'L3MON4D3/LuaSnip',
      dependencies = {
         'rafamadriz/friendly-snippets',
      },
      opts = {
         -- history = true,
         delete_check_events = 'TextChanged',
      },
      keys = {
         {
            '<tab>',
            function()
               return require('luasnip').jumpable(1) and '<Plug>luasnip-jump-next' or '<tab>'
            end,
            expr = true,
            silent = true,
            mode = 'i',
         },
         {
            '<tab>',
            function()
               require('luasnip').jump(1)
            end,
            mode = 's',
         },
         {
            '<s-tab>',
            function()
               require('luasnip').jump(-1)
            end,
            mode = { 'i', 's' },
         },
      },
   },

   { -- Autocompletion
      'hrsh7th/nvim-cmp',
      event = 'InsertEnter',
      dependencies = {
         'hrsh7th/cmp-nvim-lsp',
         'L3MON4D3/LuaSnip',
         'saadparwaiz1/cmp_luasnip',
         'onsails/lspkind.nvim',
      },
      opts = function()
         local cmp = require 'cmp'
         local luasnip = require 'luasnip'
         require('luasnip.loaders.from_vscode').lazy_load()

         return {
            preselect = cmp.PreselectMode.None,
            window = {
               -- completion = cmp.config.window.bordered(),
               documentation = cmp.config.window.bordered(),
            },
            snippet = {
               expand = function(args)
                  luasnip.lsp_expand(args.body)
               end,
            },
            mapping = cmp.mapping.preset.insert {
               ['<C-d>'] = cmp.mapping.scroll_docs(-4),
               ['<C-f>'] = cmp.mapping.scroll_docs(4),
               ['<C-Space>'] = cmp.mapping.complete(),
               ['<Tab>'] = cmp.mapping.confirm {
                  behavior = cmp.ConfirmBehavior.Replace,
                  select = true,
               },
               ['<CR>'] = cmp.mapping.confirm {
                  behavior = cmp.ConfirmBehavior.Replace,
                  select = true,
               },
               -- ['<Tab>'] = cmp.mapping(function(fallback)
               --    if cmp.visible() then
               --       cmp.confirm { behavior = cmp.ConfirmBehavior.Replace, select = true }
               --       -- cmp.select_next_item()
               --    elseif luasnip.expand_or_jumpable() then
               --       luasnip.expand_or_jump()
               --    elseif has_words_before() then
               --       cmp.complete()
               --    else
               --       fallback()
               --    end
               -- end, { 'i', 's' }),
               -- ['<S-Tab>'] = cmp.mapping(function(fallback)
               --    if cmp.visible() then
               --       cmp.select_prev_item()
               --    elseif luasnip.jumpable(-1) then
               --       luasnip.jump(-1)
               --    else
               --       fallback()
               --    end
               -- end, { 'i', 's' }),
            },
            sources = {
               { name = 'luasnip' },
               { name = 'nvim_lsp' },
            },
            formatting = {
               format = require('lspkind').cmp_format {
                  mode = 'symbol_text',
                  maxwidth = 60,
                  ellipsis_char = '...',
               },
            },
         }
      end,
   },

   {
      'windwp/nvim-autopairs',
      event = 'VeryLazy',
      config = function()
         require('nvim-autopairs').setup {}
      end,
   },

   {
      'kylechui/nvim-surround',
      event = 'VeryLazy',
      config = true,
   },

   { -- Highlight, edit, and navigate code
      'nvim-treesitter/nvim-treesitter',
      event = { 'BufReadPost', 'BufNewFile' },
      dependencies = { 'nvim-treesitter/nvim-treesitter-textobjects' },
      build = function()
         pcall(require('nvim-treesitter.install').update { with_sync = true })
      end,

      opts = {
         -- Add languages to be installed here that you want installed for treesitter
         ensure_installed = {
            'bash',
            'c',
            'go',
            'help',
            'html',
            'javascript',
            'json',
            'jsonc',
            'lua',
            'markdown',
            'markdown_inline',
            'python',
            'rust',
            'tsx',
            'typescript',
            'vim',
            'yaml',
         },
         highlight = { enable = true },
         indent = { enable = true },
         textobjects = {
            select = {
               enable = true,
               lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
               keymaps = {
                  -- You can use the capture groups defined in textobjects.scm
                  ['aa'] = '@parameter.outer',
                  ['ia'] = '@parameter.inner',
                  ['af'] = '@function.outer',
                  ['if'] = '@function.inner',
                  ['ac'] = '@class.outer',
                  ['ic'] = '@class.inner',
               },
            },
            move = {
               enable = true,
               set_jumps = true, -- whether to set jumps in the jumplist
               goto_next_start = {
                  [']m'] = '@function.outer',
                  [']]'] = '@class.outer',
               },
               goto_next_end = {
                  [']M'] = '@function.outer',
                  [']['] = '@class.outer',
               },
               goto_previous_start = {
                  ['[m'] = '@function.outer',
                  ['[['] = '@class.outer',
               },
               goto_previous_end = {
                  ['[M'] = '@function.outer',
                  ['[]'] = '@class.outer',
               },
            },
         },
      },
      config = function(_, opts)
         require('nvim-treesitter.configs').setup(opts)
      end,
   },

   {
      'lewis6991/gitsigns.nvim',
      event = { 'BufReadPre', 'BufNewFile' },
      opts = {
         signs = {
            add = { text = '▎' },
            change = { text = '▎' },
            delete = { text = '契' },
            topdelete = { text = '契' },
            changedelete = { text = '▎' },
            untracked = { text = '▎' },
         },
      },
   },

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
         -- highlight_overrides = {
         --    mocha = function(mocha)
         --       return {
         --          StatusLine = { fg = mocha.text },
         --       }
         --    end,
         -- },
      },
   },

   {
      'numToStr/Comment.nvim',
      event = 'VeryLazy',
      dependencies = { 'JoosepAlviste/nvim-ts-context-commentstring' },
      config = function()
         require('Comment').setup {
            pre_hook = require('ts_context_commentstring.integrations.comment_nvim').create_pre_hook(),
         }
      end,
   },

   { -- Fuzzy Finder (files, lsp, etc)
      'nvim-telescope/telescope.nvim',
      branch = '0.1.x',
      dependencies = { 'nvim-lua/plenary.nvim', 'nvim-telescope/telescope-file-browser.nvim' },
      cmd = 'Telescope',
      config = function()
         local actions = require 'telescope.actions'
         require('telescope').setup {
            defaults = {
               layout_config = {
                  horizontal = {
                     preview_width = 0.50,
                  },
               },
               mappings = {
                  i = {
                     ['<C-u>'] = false,
                     ['<C-d>'] = false,
                     ['<esc>'] = actions.close,
                     ['kj'] = actions.close,
                     ['<C-g>'] = actions.close,
                     ['<C-f>'] = actions.preview_scrolling_down,
                     ['<C-b>'] = actions.preview_scrolling_up,
                  },
               },
            },
            pickers = {
               find_files = {
                  find_command = { 'rg', '--files', '--hidden', '--glob', '!**/.git/*' },
               },
            },
            extensions = {
               file_browser = {
                  hidden = true,
                  select_buffer = true,
                  respect_gitignore = true,
               },
            },
         }
         require('telescope').load_extension 'file_browser'
      end,
      keys = function()
         local builtin = require 'telescope.builtin'
         return {
            { '<leader><space>', builtin.commands, desc = 'commands' },
            { '<leader>:', builtin.command_history, desc = 'command history' },
            -- {
            --    '<leader>ff',
            --    function()
            --       builtin.find_files { cwd = vim.fn.expand '%:p:h' }
            --    end,
            --    desc = 'find files',
            -- },
            { '<leader>ff', ':Telescope file_browser path=%:p:h<cr>', desc = 'browse files' },
            { '<leader>pf', builtin.find_files, desc = 'find project files' },
            { '<leader>fr', builtin.oldfiles, desc = 'recent files' },
            { '<leader>sg', builtin.live_grep, desc = 'live grep' },
            {
               '<leader>bb',
               function()
                  builtin.buffers { sort_lastused = true, ignore_current_buffer = true }
               end,
               desc = 'switch buffer',
            },
            { '<leader>hh', builtin.help_tags, desc = 'search help' },
            { '<leader>hk', builtin.keymaps, desc = 'keymaps' },
            { '<leader>hl', builtin.highlights, desc = 'highlights' },
            { '<leader>cR', builtin.lsp_references, desc = 'lsp references' },
         }
      end,
   },

   { -- Fuzzy Finder Algorithm which requires local dependencies to be built. Only load if `make` is available
      'nvim-telescope/telescope-fzf-native.nvim',
      build = 'make',
      dependencies = { 'nvim-telescope/telescope.nvim' },
      cond = vim.fn.executable 'make' == 1,
      config = function()
         require('telescope').load_extension 'fzf'
      end,
   },

   -- { -- which-key
   --   "folke/which-key.nvim",
   --   event = "VeryLazy",
   --   opts = {
   --     plugins = { spelling = true },
   --   },
   --   config = function(_, opts)
   --     local wk = require("which-key")
   --     wk.setup(opts)
   --     wk.register({
   --       mode = { "n", "v" },
   --       ["g"] = { name = "+goto" },
   --       ["]"] = { name = "+next" },
   --       ["["] = { name = "+prev" },
   --       ["<leader><tab>"] = { name = "+tabs" },
   --       ["<leader>b"] = { name = "+buffer" },
   --       ["<leader>c"] = { name = "+code" },
   --       ["<leader>f"] = { name = "+files" },
   --       ["<leader>g"] = { name = "+git" },
   --       ["<leader>q"] = { name = "+quit/session" },
   --       ["<leader>s"] = { name = "+search" },
   --       ["<leader>w"] = { name = "+windows" },
   --       -- ["<leader>x"] = { name = "+diagnostics/quickfix" },
   --     })
   --   end,
   -- },

   {
      'max397574/better-escape.nvim',
      event = 'InsertEnter',
      opts = {
         mapping = { 'kj' },
      },
   },

   {
      'aserowy/tmux.nvim',
      config = function()
         return require('tmux').setup {
            copy_sync = {
               sync_registers = false,
            },
         }
      end,
   },

   'google/vim-searchindex',

   {
      'phaazon/hop.nvim',
      event = 'VeryLazy',
      branch = 'v2',
      config = function()
         return require('hop').setup {}
      end,
      keys = {
         { 's', ':HopChar2<cr>' },
      },
   },

   {
      'stevearc/dressing.nvim',
      event = 'VeryLazy',
      config = function()
         require('dressing').setup()
      end,
   },
   {
      'folke/trouble.nvim',
      dependencies = 'nvim-tree/nvim-web-devicons',
      config = function()
         require('trouble').setup {
            height = 10,
            padding = false,
         }
      end,
      keys = {
         { '<leader>cx', ':TroubleToggle<cr>' },
      },
   },
   {
      'klen/nvim-test',
      event = 'VeryLazy',
      opts = {
         termOpts = {
            width = 80,
            go_back = true,
         },
      },
      keys = {
         { '<leader>ta', ':TestSuite<cr>' },
         { '<leader>tf', ':TestFile<cr>' },
         { '<leader>tt', ':TestNearest<cr>' },
         { '<leader>tl', ':TestLast<cr>' },
         { '<leader>tb', ':TestVisit<cr>' },
      },
   },
   {
      'stevearc/oil.nvim',
      event = 'VeryLazy',
      config = function()
         require('oil').setup {
            columns = { 'icon', 'size', 'mtime' },
            keymaps = {
               ['g?'] = 'actions.show_help',
               ['<CR>'] = 'actions.select',
               ['O'] = 'actions.select_vsplit',
               ['B'] = 'actions.select_split',
               ['p'] = 'actions.preview',
               ['q'] = 'actions.close',
               ['-'] = 'actions.parent',
               ['_'] = 'actions.open_cwd',
               ['g.'] = 'actions.toggle_hidden',
            },
            use_default_keymaps = false,
            view_options = {
               show_hidden = true,
            },
         }
      end,
      keys = {
         { '<leader>fd', ':Oil<cr>' },
      },
   },
   {
      'sindrets/diffview.nvim',
      dependencies = 'nvim-lua/plenary.nvim',
      config = function()
         require('diffview').setup {}
      end,
   },
   {
      'nvim-lualine/lualine.nvim',
      event = 'VeryLazy',
      -- dependencies = {
      --    'linrongbin16/lsp-progress.nvim',
      -- },
      config = function()
         require('lualine').setup {
            options = {
               section_separators = '',
               component_separators = '',
               theme = 'catppuccin',
               globalstatus = true,
            },
            sections = {
               lualine_a = {},
               lualine_b = {},
               lualine_c = { 'diagnostics', 'filename', 'branch' },
               lualine_x = {
                  {
                     'fileformat',
                     symbols = {
                        unix = '',
                        dos = '[dos]',
                        mac = '[mac]',
                     },
                  },
                  'location',
                  'filetype',
               },
               lualine_y = {},
               lualine_z = {},
            },
         }
      end,
   },
   {
      'scalameta/nvim-metals',
      dependencies = { 'nvim-lua/plenary.nvim' },
      config = function()
         local metals_config = require('metals').bare_config()

         metals_config.settings = {
            showImplicitArguments = true,
            excludedPackages = { 'akka.actor.typed.javadsl', 'com.github.swagger.akka.javadsl' },
         }
         -- *READ THIS*
         -- I *highly* recommend setting statusBarProvider to true, however if you do,
         -- you *have* to have a setting to display this in your statusline or else
         -- you'll not see any messages from metals. There is more info in the help
         -- docs about this
         metals_config.init_options.statusBarProvider = 'on'
         metals_config.handlers['metals/status'] = metals_status_handler
         local capabilities = vim.lsp.protocol.make_client_capabilities()
         metals_config.capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)
         metals_config.on_attach = on_attach
         local nvim_metals_group = vim.api.nvim_create_augroup('nvim-metals', { clear = true })

         vim.api.nvim_create_autocmd('FileType', {
            pattern = { 'scala', 'sbt', 'java' },
            callback = function()
               require('metals').initialize_or_attach(metals_config)
            end,
            group = nvim_metals_group,
         })
      end,
   },
   {
      'simrat39/rust-tools.nvim',
      config = function()
         require('rust-tools').setup {
            server = {
               on_attach = on_attach,
            },
         }
      end,
   },
   -- {
   --    'akinsho/toggleterm.nvim',
   --    version = '*',
   --    config = function()
   --       require('toggleterm').setup {
   --          open_mapping = [[<c-\>]],
   --       }
   --
   --       function _G.set_terminal_keymaps()
   --          local opts = { buffer = 0 }
   --          map('t', '<esc>', [[<C-\><C-n>]], opts)
   --          map('t', 'jk', [[<C-\><C-n>]], opts)
   --          map('t', '<C-h>', [[<Cmd>wincmd h<CR>]], opts)
   --          map('t', '<C-j>', [[<Cmd>wincmd j<CR>]], opts)
   --          map('t', '<C-k>', [[<Cmd>wincmd k<CR>]], opts)
   --          map('t', '<C-l>', [[<Cmd>wincmd l<CR>]], opts)
   --          map('t', '<C-w>', [[<C-\><C-n><C-w>]], opts)
   --       end
   --
   --       map('n', '<leader>m1', '<cmd>1ToggleTerm<cr>')
   --       map('n', '<leader>m2', '<cmd>2ToggleTerm<cr>')
   --       map('n', '<leader>m3', '<cmd>3ToggleTerm<cr>')
   --       map('n', '<leader>m4', '<cmd>4ToggleTerm<cr>')
   --       map('n', '<leader>m5', '<cmd>5ToggleTerm<cr>')
   --
   --       vim.cmd 'autocmd! TermOpen term://* lua set_terminal_keymaps()'
   --    end,
   -- },

   {
      'ahmedkhalf/project.nvim',
      config = function()
         require('project_nvim').setup {
            exclude_dirs = {
               '/home/aki',
            },
         }

         require('telescope').load_extension 'projects'
         map('n', '<leader>pp', '<cmd>Telescope projects<cr>', { desc = 'switch projects' })
      end,
   },
}, {
   ui = {
      border = 'rounded',
   },
})

-- [[ Setting options ]]
-- See `:help vim.o`

-- Set highlight on search
vim.o.hlsearch = true

-- vim.o.showmode = false

-- max autocomplete height
vim.o.pumheight = 17

-- System clipboard
vim.o.clipboard = 'unnamedplus'

-- Enable mouse mode
vim.o.mouse = 'a'

-- Enable break indent
vim.o.breakindent = true

-- Save undo history
vim.o.undofile = true

-- No swapfile
vim.o.swapfile = false

-- Case insensitive searching UNLESS /C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

-- Set colorscheme
vim.o.termguicolors = true
vim.cmd [[colorscheme catppuccin]]

-- hide search anzu
vim.cmd [[set shortmess +=S]]

-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menuone,noselect'

-- Use spaces instead of tabs
vim.o.expandtab = true
vim.o.smarttab = true

-- Default tab width is 2
vim.o.tabstop = 2
vim.o.shiftwidth = 2
-- vim.bo.softtabstop = 2
vim.o.smartindent = true

-- Preview incremental substitute
vim.o.inccommand = true

-- Make more sense spliting
vim.o.splitbelow = true -- Put new windows below current
vim.o.splitright = true -- Put new windows right of current

-- Only one statusline
vim.o.laststatus = 3

-- Show lists
vim.o.list = true
vim.o.listchars = 'tab:│ '

-- Discontinue comments on new line
vim.api.nvim_create_autocmd('BufEnter', {
   callback = function()
      vim.opt.formatoptions = vim.opt.formatoptions - { 'c', 'r', 'o' }
   end,
})

vim.api.nvim_create_autocmd('FileType', {
   pattern = 'lua',
   command = 'setlocal shiftwidth=3 tabstop=3',
})

vim.api.nvim_create_autocmd('FileType', {
   pattern = 'go',
   command = 'setlocal shiftwidth=4 tabstop=4',
})

-- [[ Basic Keymaps ]]
map('', ';', ':')
map('c', 'kj', '<esc>')

-- Keymaps for better default experience
-- See `:help vim.keymap.set()`
map({ 'n', 'v' }, '<Space>', '<Nop>')

-- Remap for dealing with word wrap
map('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true })
map('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true })

-- Better indents
map('v', '>', '>gv')
map('v', '<', '<gv')

-- Buffers
map('n', '[b', ':bn<cr>')
map('n', ']b', ':bp<cr>')
map('n', '<leader>bd', ':bd<cr>')
map('n', '<leader>bn', ':bn<cr>')
map('n', '<leader>bp', ':bp<cr>')
map('n', '<leader>bl', ':e #<cr>')

-- Files
map('n', '<leader>fi', ':e ~/.config/nvim/init.lua<cr>')
map('n', '<leader>ft', ':e ~/.config/kitty/kitty.conf<cr>')
map('n', '<leader>fz', ':e ~/.zshrc<cr>')

-- Windows
map('n', '<leader>wo', '<C-W>o')
map('n', '<leader>wd', '<C-W>c')
map('n', '<leader>wh', '<C-W>h')
map('n', '<leader>wj', '<C-W>j')
map('n', '<leader>wk', '<C-W>k')
map('n', '<leader>wl', '<C-W>l')
map('n', '<leader>wv', '<C-W>v')
map('n', '<leader>ws', '<C-W>s')

local function augroup(name)
   return vim.api.nvim_create_augroup(name, { clear = true })
end

-- Highlight on yank
vim.api.nvim_create_autocmd('TextYankPost', {
   group = augroup 'HighlightYank',
   callback = function()
      vim.highlight.on_yank()
   end,
})

-- Go to last loc when opening a buffer
vim.api.nvim_create_autocmd('BufReadPost', {
   group = augroup 'LastLoc',
   callback = function()
      local mark = vim.api.nvim_buf_get_mark(0, '"')
      local lcount = vim.api.nvim_buf_line_count(0)
      if mark[1] > 0 and mark[1] <= lcount then
         pcall(vim.api.nvim_win_set_cursor, 0, mark)
      end
   end,
})

-- close some filetypes with <q>
vim.api.nvim_create_autocmd('FileType', {
   group = augroup 'CloseWithQ',
   pattern = {
      'qf',
      'help',
      'man',
      'notify',
      'lspinfo',
      'spectre_panel',
      'startuptime',
      'tsplayground',
      'PlenaryTestPopup',
   },
   callback = function(event)
      vim.bo[event.buf].buflisted = false
      vim.keymap.set('n', 'q', '<cmd>close<cr>', { buffer = event.buf, silent = true })
   end,
})

vim.api.nvim_create_autocmd('BufWritePre', {
   pattern = { '*.go' },
   callback = function()
      goimports(1000)
   end,
})

-- vim.cmd [[set statusline=%!v:lua.require'statusline'.statusline()]]
-- The line beneath this is called `modeline`. See `:help modeline`
-- vim: ts=3 sts=3 sw=3 et
