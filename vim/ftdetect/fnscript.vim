au BufRead,BufNewFile *.fns set filetype=fnscript
au BufRead,BufNewFile * if getline(1) =~ '^#!.*fn-\?script' | set filetype=fnscript | endif
