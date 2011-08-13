" langage specific prefixes
let s:prefix = {'javascript': '\/\/ ', 'java': '\/\/ ', 'vim': '\" ', 'ruby': '# '}

function s:do()
   if has_key(s:prefix, b:current_syntax)
      exe 's/^/'.s:prefix[b:current_syntax].'/'
      exe 'noh'
      silent! call repeat#set("\<Plug>MuCommentDo",v:count)
   endif
endfunction
noremap <silent><Plug>MuCommentDo :call <SID>do()<CR>

function s:undo()
   if has_key(s:prefix, b:current_syntax)
      exe 'silent!s/^'.s:prefix[b:current_syntax].'//'
      exe 'noh'
      silent! call repeat#set("\<Plug>MuCommentUndo",v:count)
   endif
endfunction
noremap <silent><Plug>MuCommentUndo :call <SID>undo()<CR>

" default mappings
map <Leader>c <Plug>MuCommentDo
map <Leader>C <Plug>MuCommentUndo
