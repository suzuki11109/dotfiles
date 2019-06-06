let mapleader = "\<space>"
"""""" Plugins

call plug#begin()
Plug 'tomasiser/vim-code-dark'
Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tomtom/tcomment_vim'
Plug 'tmux-plugins/vim-tmux-focus-events'
Plug 'christoomey/vim-tmux-navigator'
Plug 'janko-m/vim-test'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-unimpaired'
Plug 'cohama/lexima.vim'
Plug 'pangloss/vim-javascript', { 'for': ['javascript'] }
Plug 'MaxMEllon/vim-jsx-pretty', { 'for': ['javascript'] }
Plug 'mattn/emmet-vim', { 'for': ['html', 'javascript', 'typescript'] }
Plug 'hail2u/vim-css3-syntax', { 'for': ['javascript', 'css'] }
Plug 'tpope/vim-fireplace', { 'for': ['clojure'] }
Plug 'Chiel92/vim-autoformat'
Plug 'w0rp/ale'
Plug 'lifepillar/vim-mucomplete'
Plug 'autozimu/LanguageClient-neovim', {
      \ 'branch': 'next',
      \ 'do': 'bash install.sh',
      \ }
call plug#end()


au FileType go au BufWrite <buffer> Autoformat
let g:formatdef_goimports = '"goimports"'
let g:formatters_go = ['goimports']

" mucomplete
set completeopt-=preview
set completeopt+=menuone
set shortmess+=c
set belloff+=ctrlg
set complete-=t
set complete-=i
let g:mucomplete#buffer_relative_paths = 1
let g:mucomplete#can_complete = {
      \ 'default': { 'omni': { t -> t =~# '\%(\k\|\k\.\)$'  } },
      \ 'javascript': { 'omni': { t -> t =~# '\%(\k\|\k\.\|/\)$'  } },
      \ 'javascript.jsx': { 'omni': { t -> t =~# '\%(\k\|\k\.\|/\)$'  } },
      \ 'typescript': { 'omni': { t -> t =~# '\%(\k\|\k\.\|/\)$'  } },
      \ }


let g:mucomplete#chains = {
      \ 'javascript' : ['omni', 'keyn', 'uspl'],
      \ 'javascript.jsx' : ['omni', 'keyn', 'uspl'],
      \ 'typescript' : ['omni', 'keyn', 'uspl'],
      \ }

let g:LanguageClient_serverCommands = {
      \ 'javascript.jsx': ['typescript-language-server', '--stdio'],
      \ 'javascript': ['typescript-language-server', '--stdio'],
      \ 'typescript': ['typescript-language-server', '--stdio'],
      \ 'go': ['gopls'],
      \ }


let g:LanguageClient_rootMarkers = {
      \ 'typescript': ['package.json'],
      \ 'javascript': ['package.json'],
      \ 'javascript.jsx': ['package.json'],
      \ 'go': ['Gopkg.toml', 'go.mod', '.vim/', '.git/', '.hg/'],
      \ }

let g:LanguageClient_diagnosticsEnable = 0
nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>
nnoremap <silent> gR :call LanguageClient#textDocument_rename()<CR>

" vim-test
let test#strategy = "neovim"
nnoremap <leader>nh :noh<CR>
nnoremap <leader>tf :TestFile<CR>
nnoremap <leader>tt :TestNearest<CR>
nnoremap <leader>ta :TestSuite<CR>

" ale
set signcolumn=yes
let g:ale_sign_error = '✖'
let g:ale_sign_warning = '!'
let g:ale_linters = {
      \  'javascript': ['eslint'],
      \  'typescript': ['tslint', 'tsserver'],
      \  'go': ['gopls'],
      \}
let g:ale_set_highlights = 0
nnoremap ]c :ALENext<CR>
nnoremap [c :ALEPrevious<CR>

" clojure
autocmd FileType clojure nnoremap <buffer> <leader>re :Eval<cr>
autocmd FileType clojure vnoremap <buffer> <leader>re :Eval<cr>
autocmd FileType clojure nnoremap <buffer> <leader>rf :%Eval<cr>
autocmd FileType clojure nnoremap <buffer> <leader>rr :Require<cr>
autocmd FileType clojure nnoremap <buffer> <leader>rR :Require!<cr>
autocmd FileType clojure nnoremap <buffer> <leader>rt :RunTests<cr>
autocmd FileType clojure nnoremap <buffer> <leader>rl :Last<cr>
autocmd FileType clojure nnoremap <buffer> <leader>rc :FireplaceConnect<cr>
autocmd FileType clojure nnoremap <buffer> gd :normal [<c-d><cr>

""""" End Plugins

"""""" Basics
filetype plugin indent on
syntax enable
set tabstop=2
set softtabstop=2
set expandtab
set shiftwidth=2

set autoindent
set smartindent
set backspace=indent,eol,start

set hlsearch
set incsearch
set ignorecase
set smartcase
set title

set number
set clipboard=unnamed

set laststatus=2
set wildmenu

set encoding=utf-8

set wrap

set splitbelow
set splitright

set noswapfile

set hidden
set autoread
set nobackup
set nowritebackup

set foldlevel=0
set foldmethod=manual

set list lcs=tab:\|\ ,trail:·

" disable autocomment when new line
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

au FileType go setlocal tabstop=4 shiftwidth=4

"""""" End Basics

"""""" Functions
function! s:find_git_root()
  return system('git rev-parse --show-toplevel 2> /dev/null')[:-2]
endfunction

command! ProjectFiles execute 'Files' s:find_git_root()
command! FindFiles execute 'Files' expand('%:p:h')

command! -bang -nargs=* Rg
      \ call fzf#vim#grep(
      \   'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
      \   <bang>0 ? fzf#vim#with_preview('up:60%')
      \           : fzf#vim#with_preview('right:50%:hidden', '?'),
      \   <bang>0)

function! LinterStatus() abort
  let l:counts = ale#statusline#Count(bufnr('%'))
  let l:all_errors = l:counts.error + l:counts.style_error
  let l:all_non_errors = l:counts.total - l:all_errors

  return printf(
        \   'W:%d E:%d',
        \   all_non_errors,
        \   all_errors
        \)
endfunction

"""""" End Functions

"""""" UI
set background=dark
set termguicolors
colorscheme codedark

hi link jsxTagName xmlTag
hi link jsxComponentName xmlTag

set statusline=
set statusline+=%0*%n
set statusline+=\ %*
set statusline+=%0*\ %t\ %*
set statusline+=%0*\%m%R
set statusline+=\ %{FugitiveStatusline()}
set statusline+=%0*\ %{LinterStatus()}
set statusline+=%=
set statusline+=%0*\ %l:%c
set statusline+=%0*\ %{(&fenc!=''?&fenc:&enc)}
set statusline+=%0*\ %y\ %*
"""""" End UI

"""""" Misc

au BufWritePre * :%s/\s\+$//e
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

nmap <leader>pf :Files<CR>
nmap <leader>ff :FindFiles<CR>
nmap <leader>bb :Buffers<CR>
nmap <silent> <leader>bd :bp\|bd #<CR>

nnoremap J mzJ`z
nnoremap Q <Nop>
nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk
xnoremap <  <gv
xnoremap >  >gv
inoremap <C-U> <C-G>u<C-U>
nnoremap <space>l <C-^><CR>
nnoremap <expr> dd match(getline('.'), '^\s*$') == -1 ? 'dd' : '"_dd'
tmap <C-o> <C-\><C-n>

" inoremap '<tab> ''<left>
" inoremap `<tab> ``<left>
" inoremap "<tab> ""<left>
" inoremap (<tab> ()<left>
" inoremap [<tab> []<left>
" inoremap {<tab> {}<left>
"
" inoremap ';<tab> '';<left><left>
" inoremap `;<tab> ``;<left><left>
" inoremap ";<tab> "";<left><left>
" inoremap (;<tab> ();<left><left>
" inoremap [;<tab> [];<left><left>
" inoremap {;<tab> {};<left><left>
"
" inoremap ', '',<left><left>
" inoremap `, ``,<left><left>
" inoremap ", "",<left><left>
" inoremap (, (),<left><left>
" inoremap [, [],<left><left>
" inoremap {, {},<left><left>

" inoremap '<tab> ''
" inoremap `<tab> ``
" inoremap "<tab> ""
" inoremap (<tab> ()
" inoremap [<tab> []
" inoremap {<tab> {}

" inoremap ';<tab> '';
" inoremap `;<tab> ``;
" inoremap ";<tab> "";
" inoremap (;<tab> ();
" inoremap [;<tab> [];
" inoremap {;<tab> {};

" inoremap ',<tab> '',
" inoremap `,<tab> ``,
" inoremap ",<tab> "",
" inoremap (,<tab> (),
" inoremap [,<tab> [],
" inoremap {,<tab> {},

" inoremap (<CR> (<CR>)<ESC>O
" inoremap [<CR> [<CR>]<ESC>O
" inoremap {<CR> {<CR>}<ESC>O

"autoclose 2 lines below adding ; and position cursor in the middle
" inoremap (;<CR> (<CR>);<ESC>O
" inoremap [;<CR> [<CR>];<ESC>O
" inoremap {;<CR> {<CR>};<ESC>O

"autoclose 2 lines below adding , and position cursor in the middle
" inoremap (,<CR> (<CR>),<ESC>O
" inoremap [,<CR> [<CR>],<ESC>O
" inoremap {,<CR> {<CR>},<ESC>O
"
"""""" End Misc
