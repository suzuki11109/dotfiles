local M = {}

function M.output()
   local parts = {
      '%<%f ',
      '%h%m%r ',
      -- '%#LspDiagnosticsError#',
      -- [[%{luaeval("require'statusline'.diagnostic_errors()")}]],
      -- '%*',
      -- '%#LspDiagnosticsWarning#',
      -- [[%{luaeval("require'statusline'.diagnostic_warnings()")}]],
      -- '%*',
      -- '%#LspDiagnosticsHint#',
      -- [[%{luaeval("require'statusline'.diagnostic_hints()")}]],
      -- '%*',
      -- '%#LspDiagnosticsInformation#',
      -- [[%{luaeval("require'statusline'.diagnostic_infos()")}]],
      -- '%*',
      '%=',
      -- " %{get(b:,'gitsigns_head','')}",
      ' %l,%c%V',
      ' %P',
      "%{&ff!='unix'?&ff:''}",
      ' %y',
   }
   return table.concat(parts, '')
end

function M.diagnostic_errors()
   local num_errors = #vim.diagnostic.get(nil, { severity = vim.diagnostic.severity.ERROR })
   if num_errors > 0 then
      return ' ' .. num_errors .. ' '
   end
   return ''
end

function M.diagnostic_warnings()
   local num = #vim.diagnostic.get(nil, { severity = vim.diagnostic.severity.WARN })
   if num > 0 then
      return ' ' .. num .. ' '
   end
   return ''
end

function M.diagnostic_hints()
   local num = #vim.diagnostic.get(nil, { severity = vim.diagnostic.severity.HINT })
   if num > 0 then
      return ' ' .. num .. ' '
   end
   return ''
end

function M.diagnostic_infos()
   local num = #vim.diagnostic.get(nil, { severity = vim.diagnostic.severity.INFO })
   if num > 0 then
      return ' ' .. num .. ' '
   end
   return ''
end

return M
