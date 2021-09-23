" Support functions {{{
function! s:setf(filetype) abort
  if &filetype !~# '\<'.a:filetype.'\>'
    let &filetype = a:filetype
  endif
endfunction

autocmd BufNewFile,BufRead *.rbs call s:setf('rbs')

" vim: nowrap sw=2 sts=2 ts=8 noet fdm=marker:
