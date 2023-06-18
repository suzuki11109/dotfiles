local M = {}

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
   -- Diagnostic keymaps

   -- wk.register({
   --    r = { vim.lsp.buf.rename, 'rename' },
   --    a = { vim.lsp.buf.code_action, 'code action' },
   -- }, { prefix = '<leader>c' })

   -- map('n', '[e', function()
   --    vim.diagnostic.goto_prev { float = false }
   -- end, { desc = 'previous diagnostic' })
   -- map('n', ']e', function()
   --    vim.diagnostic.goto_next { float = false }
   -- end, { desc = 'next diagnostic' })

   -- map({ 'n', 'v' }, '<leader>cr', vim.lsp.buf.rename, { desc = 'rename' })
   -- map({ 'n', 'v' }, '<leader>ca', vim.lsp.buf.code_action, { desc = 'code action' })
   -- map({ 'n', 'v' }, 'gd', vim.lsp.buf.definition, { desc = 'goto definition' })
   -- map({ 'n', 'v' }, 'gI', vim.lsp.buf.implementation, { desc = 'goto implementation' })

   -- telescope helper
   -- map('n', '<leader>cR', '<cmd>Telescope lsp_references<cr>', { desc = 'find references' })
   -- map('n', '<leader>cs', '<cmd>Telescope lsp_document_symbols<cr>', { desc = 'document symbols' })
   --
   -- map({ 'n', 'v' }, 'K', vim.lsp.buf.hover, { desc = 'Hover Documentation' })
   -- map({ 'n', 'v' }, 'K', hover_doc, { desc = 'Hover Documentation' })

   -- vim.lsp.handlers['textDocument/hover'] = vim.lsp.with(vim.lsp.handlers.hover, {
   --    border = 'rounded',
   -- })

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

   -- vim.api.nvim_create_autocmd('BufWritePre', {
   --    pattern = { '*.go' },
   --    callback = function()
   --       goimports(1000)
   --    end,
   -- })
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

-- local function get_preview_window()
--    for _, win in ipairs(vim.api.nvim_tabpage_list_wins(vim.api.nvim_get_current_tabpage())) do
--       if vim.wo[win].previewwindow then
--          return win
--       end
--    end
-- end

-- local function create_preview_window()
--    vim.cmd.new()
--    vim.cmd.stopinsert()
--    local pwin = vim.api.nvim_get_current_win()
--    vim.wo[pwin].previewwindow = true
--    vim.wo[pwin].relativenumber = false
--    vim.wo[pwin].number = false
--    vim.api.nvim_win_set_height(pwin, vim.api.nvim_get_option 'previewheight')
--    local pbuf = vim.api.nvim_win_get_buf(pwin)
--    -- vim.bo[pbuf].bufhidden = 'delete'
--    vim.bo[pbuf].buflisted = false
--    vim.bo[pbuf].buftype = 'nofile'
--    vim.api.nvim_command [[ nnoremap <silent> <buffer>q :set nopreviewwindow<CR>:bd<CR> ]]
--    return pwin
-- end

-- function M.hover_doc()
--    vim.lsp.buf_request(0, 'textDocument/hover', vim.lsp.util.make_position_params(), function(_, result, ctx, config)
--       config = config or {}
--       config.focus_id = ctx.method
--       if not (result and result.contents) then
--          vim.notify 'No information available'
--          return
--       end
--       local doc_lines = vim.lsp.util.convert_input_to_markdown_lines(result.contents)
--       doc_lines = vim.lsp.util.trim_empty_lines(doc_lines)
--       if vim.tbl_isempty(doc_lines) then
--          vim.notify 'No information available'
--          return
--       end
--
--       local pwin = get_preview_window() or create_preview_window()
--       local pbuf = vim.api.nvim_win_get_buf(pwin)
--       -- set markdown types
--       vim.bo[pbuf].filetype = 'markdown'
--       vim.wo[pwin].conceallevel = 2
--       vim.wo[pwin].concealcursor = vim.wo[pwin].concealcursor .. 'n'
--       -- replace contents
--       vim.bo[pbuf].modifiable = true
--       vim.api.nvim_buf_set_lines(pbuf, 0, 1, false, doc_lines)
--       -- disable insert mode for this preview buffer
--       vim.bo[pbuf].modifiable = false
--    end)
-- end

return M
