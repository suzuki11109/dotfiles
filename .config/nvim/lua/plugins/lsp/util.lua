local M = {}

local lsp_format_augroup = vim.api.nvim_create_augroup('LspFormat', {})

local function lsp_format(bufnr)
   vim.lsp.buf.format {
      filter = function(client)
         -- always use null-ls format
         return client.name == 'null-ls'
      end,
      bufnr = bufnr,
   }
end

function M.on_attach(client, bufnr)
   vim.diagnostic.config {
      virtual_text = false,
   }

   local signs = { Error = ' ', Warn = ' ', Hint = ' ', Info = ' ' }
   for type, icon in pairs(signs) do
      local hl = 'DiagnosticSign' .. type
      vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
   end

   -- Disable highlighting from lsp
   client.server_capabilities.semanticTokensProvider = nil

   -- Create a command `:Format` local to the LSP buffer
   vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
      lsp_format(bufnr)
   end, { desc = 'format current buffer' })

   -- Autoformat
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

   -- Display diagnostic at point hover
   vim.api.nvim_create_autocmd('CursorHold', {
      buffer = bufnr,
      callback = function()
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

function M.metals_status_handler(_, status, ctx)
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

return M
