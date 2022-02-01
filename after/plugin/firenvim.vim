let g:firenvim_config = { 
    \ 'globalSettings': {
        \ 'alt': 'all',
        \ 'ignoreKeys': {
            \ 'all': ['<C-->'],
            \ 'normal': ['<C-1>', '<C-2>']
        \ }
    \  },
    \ 'localSettings': {
        \ '.*': {
            \ 'cmdline': 'neovim',
            \ 'content': 'text',
            \ 'priority': 0,
            \ 'selector': 'textarea',
            \ 'takeover': 'never',  
        \ },
    \ }
\ }

let fc = g:firenvim_config['localSettings']
let fc['https?://github.com/'] = { 'takeover': 'always', 'priority': 1 }

au BufEnter github.com_*.txt set filetype=markdown
