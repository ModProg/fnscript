if exists("b:current_syntax")
    finish
endif

syn keyword Visibility          pre post pub hid
syn keyword Definition          fn nextgroup=FunctionName skipwhite skipempty
syn keyword Definition          let
syn keyword Exit                break return continue exit fail
syn keyword Yield               yield error

syn keyword Conditional         match if else
syn keyword Repeat              loop while for in 
syn keyword Operator            as

syn match   Operator            "\%(+\|-\|/\|*\|=\|\^\|&\||\|!\|>\|<\|%\|&&\|||\)=\?"
syn match   Operator            "|\%(e\|eo\|oe\|o\)\?>"
syn keyword Operator            pipe pipe_err pipe_all
syn match   Special             "?"

syn match   fnsModPath         "\w\(\w\)*::[^<]"he=e-3,me=e-3
syn match   fnsModPathSep      "::"

syn match   Type                "[A-Z]\(\w\)*"

syn match   FunctionName        "\%(r#\)\=\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*" display contained
syn match   FunctionCall        "\w\(\w\)*("he=e-1,me=e-1
syn match   TypeCall            "[A-Z]\(\w\)*("he=e-1,me=e-1

syn match   fnsEscapeError   display contained /\\./
syn match   fnsEscape        display contained /\\\([nrt0\\'"]\|x\x\{2}\)/
syn match   fnsEscapeUnicode display contained /\\u{\%(\x_*\)\{1,6}}/
syn match   fnsStringContinuation display contained /\\\n\s*/
syn region  fnsString      matchgroup=fnsStringDelimiter start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=fnsExpand,fnsEscape,fnsEscapeUnicode,fnsEscapeError,fnsStringContinuation,@Spell
syn region  fnsString      matchgroup=fnsStringDelimiter start='r\z(#*\)"' end='"\z1' contains=@Spell

syn region  Comment             start="//"              end="$"
syn region  SpecialComment      start="//\%(//\@!\|!\)" end="$" contains=fnsExpand
syn region  Shebang             start="#!"              end="$"

if !exists("b:current_syntax_embed")
    let b:current_syntax_embed = 1
    syntax include @fnsEmbeded <sfile>:p:h/fnscript.vim
    unlet b:current_syntax_embed
    syn region fnsExpand matchgroup=Special start="{" end="}" contained contains=fnsMatchingBraces,@fnsEmbeded
    syn region fnsMatchingBraces start="{" end="}" contains=fnsMatchingBraces,@fnsEmbeded
endif

hi def link Shebang Comment

hi def link Definition          Keyword
hi def link Visibility          Keyword
hi def link Exit                Keyword
hi def link Yield               Keyword

hi def link fnsModPath          PreProc
hi def link fnsModPathSep       Special

hi def link FunctionName        Function
hi def link FunctionCall        Function
hi def link TypeCall            Constant

hi def link fnsEscapeError      Error
hi def link fnsEscapeUnicode    Special
hi def link fnsEscape           Special
hi def link fnsString           String
hi def link fnsStringDelimiter  String
