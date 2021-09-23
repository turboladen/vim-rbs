" Prelude {{{1
if exists('b:current_syntax')
  finish
endif


" this file uses line continuations
let s:cpo_sav = &cpoptions
set cpoptions&vim

" Folding Config {{{1
if has('folding') && exists('rbs_fold')
  setlocal foldmethod=syntax
endif

" Operators {{{1
syn match rbsDotOperator    "\.\|&\."

syn region rbsBracketOperator matchgroup=rbsOperator start="\%(\%(\w\|[^\x00-\x7F]\)[?!]\=\|[]})]\)\@2<=\[" end="]" contains=ALLBUT,@rbsNotTop

syn match rbsScopeOperator     "::"
syn match rbsSuperClassOperator  "<"  contained
syn match rbsEigenClassOperator  "<<" contained
syn match rbsLambdaOperator      "->"
syn match rbsSplatOperator     "\%([[{(|,=]\_s*\)\@<=\*"
syn match rbsSplatOperator     "\%(^\|\s\)\@1<=\*\%(\h\|[^\x00-\x7F]\|[:$@[]\)\@="
syn match rbsDoubleSplatOperator "\%([{(|,]\_s*\)\@<=\*\*"
syn match rbsDoubleSplatOperator "\s\@1<=\*\*\%(\h\|[^\x00-\x7F]\|[:$@{]\)\@="
syn match rbsProcOperator      "\%([[(|,]\_s*\)\@<=&"
syn match rbsProcOperator      "\s\@1<=&\%(\h\|[^\x00-\x7F]\|[:$@]\|->\)\@="

syn cluster rbsProperOperator contains=rbsTernaryOperator,rbsArithmeticOperator,rbsComparisonOperator,rbsBitwiseOperator,rbsBooleanOperator,rbsRangeOperator,rbsAssignmentOperator,rbsEqualityOperator,rbsDefinedOperator,rbsEnglishBooleanOperator
syn cluster rbsClassOperator  contains=rbsEigenClassOperator,rbsSuperClassOperator
syn cluster rbsPseudoOperator contains=rbsDotOperator,rbsScopeOperator,rbsEigenClassOperator,rbsSuperClassOperator,rbsLambdaOperator,rbsSplatOperator,rbsDoubleSplatOperator,rbsProcOperator
syn cluster rbsOperator   contains=rbs.*Operator

" Numbers {{{1
syn match rbsInteger "\%(\%(\w\|[^\x00-\x7F]\|[]})\"']\s*\)\@<!-\)\=\<0[xX]\x\+\%(_\x\+\)*r\=i\=\>"                     display
syn match rbsInteger "\%(\%(\w\|[^\x00-\x7F]\|[]})\"']\s*\)\@<!-\)\=\<\%(0[dD]\)\=\%(0\|[1-9]\d*\%(_\d\+\)*\)r\=i\=\>"                display
syn match rbsInteger "\%(\%(\w\|[^\x00-\x7F]\|[]})\"']\s*\)\@<!-\)\=\<0[oO]\=\o\+\%(_\o\+\)*r\=i\=\>"                     display
syn match rbsInteger "\%(\%(\w\|[^\x00-\x7F]\|[]})\"']\s*\)\@<!-\)\=\<0[bB][01]\+\%(_[01]\+\)*r\=i\=\>"                   display
syn match rbsFloat   "\%(\%(\w\|[^\x00-\x7F]\|[]})\"']\s*\)\@<!-\)\=\<\%(0\|[1-9]\d*\%(_\d\+\)*\)\.\d\+\%(_\d\+\)*r\=i\=\>"               display
syn match rbsFloat   "\%(\%(\w\|[^\x00-\x7F]\|[]})\"']\s*\)\@<!-\)\=\<\%(0\|[1-9]\d*\%(_\d\+\)*\)\%(\.\d\+\%(_\d\+\)*\)\=\%([eE][-+]\=\d\+\%(_\d\+\)*\)i\=\>" display

" Identifiers {{{1
syn match rbsClassName         "\%(\%(^\|[^.]\)\.\s*\)\@<!\<[[:upper:]]\%(\w\|[^\x00-\x7F]\)*\>\%(\s*(\)\@!" contained
syn match rbsModuleName       "\%(\%(^\|[^.]\)\.\s*\)\@<!\<[[:upper:]]\%(\w\|[^\x00-\x7F]\)*\>\%(\s*(\)\@!" contained
syn match rbsInterfaceName       "\%(\%(^\|[^.]\)\.\s*\)\@<!\<[[:upper:]]\%(_|\w\|[^\x00-\x7F]\)*\>\%(\s*(\)\@!" contained
syn match rbsConstant        "\%(\%(^\|[^.]\)\.\s*\)\@<!\<[[:upper:]]\%(\w\|[^\x00-\x7F]\)*\>\%(\s*(\)\@!"
syn match rbsClassVariable    "@@\%(\h\|[^\x00-\x7F]\)\%(\w\|[^\x00-\x7F]\)*" display
syn match rbsInstanceVariable "@\%(\h\|[^\x00-\x7F]\)\%(\w\|[^\x00-\x7F]\)*"  display
syn match rbsGlobalVariable   "$\%(\%(\h\|[^\x00-\x7F]\)\%(\w\|[^\x00-\x7F]\)*\|-.\)"
syn match rbsSymbolDelimiter  ":" contained
syn match rbsSymbol        "[]})\"':]\@1<!:\%(\^\|\~@\|\~\|<<\|<=>\|<=\|<\|===\|[=!]=\|[=!]\~\|!@\|!\|>>\|>=\|>\||\|-@\|-\|/\|\[]=\|\[]\|\*\*\|\*\|&\|%\|+@\|+\|`\)" contains=rbsSymbolDelimiter
syn match rbsSymbol        "[]})\"':]\@1<!:\$\%(-.\|[`~<=>_,;:!?/.'"@$*\&+0]\)"         contains=rbsSymbolDelimiter
syn match rbsSymbol        "[]})\"':]\@1<!:\%(\$\|@@\=\)\=\%(\h\|[^\x00-\x7F]\)\%(\w\|[^\x00-\x7F]\)*"  contains=rbsSymbolDelimiter
syn match rbsSymbol        "[]})\"':]\@1<!:\%(\h\|[^\x00-\x7F]\)\%(\w\|[^\x00-\x7F]\)*\%([?!=]>\@!\)\=" contains=rbsSymbolDelimiter

syn region rbsParentheses    start="("        end=")" contains=ALLBUT,@rbsNotTop contained containedin=rbsBlockParameterList
syn region rbsBlockParameterList start="\%(\%(\<do\>\|{\)\_s*\)\@32<=|" end="|" contains=ALLBUT,@rbsNotTop,@rbsProperOperator


syn cluster rbsGlobalVariable contains=rbsGlobalVariable,rbsPredefinedVariable,rbsGlobalVariableError


" Characters {{{1
syn match rbsCharacter "\%(\w\|[^\x00-\x7F]\|[]})\"'/]\)\@1<!\%(?\%(\\M-\\C-\|\\C-\\M-\|\\M-\\c\|\\c\\M-\|\\c\|\\C-\|\\M-\)\=\%(\\\o\{1,3}\|\\x\x\{1,2}\|\\[[:space:]]\|\\\=[^[:space:]]\)\)"
syn match rbsCharacter "\%(\w\|[^\x00-\x7F]\|[]})\"'/]\)\@1<!?\\u\%(\x\{4}\|{\x\{1,6}}\)"


" Module, Class, Method and Alias Declarations {{{1
syn match rbsAliasDeclaration  "[^[:space:];#.()]\+" contained contains=rbsSymbol,@rbsGlobalVariable nextgroup=rbsAliasDeclaration2 skipwhite
syn match rbsAliasDeclaration2 "[^[:space:];#.()]\+" contained contains=rbsSymbol,@rbsGlobalVariable
syn match rbsMethodDeclaration "[^[:space:];#(]\+"   contained contains=rbsConstant,rbsBoolean,rbsPseudoVariable,rbsInstanceVariable,rbsClassVariable,rbsGlobalVariable
syn match rbsClassDeclaration  "[^[:space:];#<]\+"   contained contains=rbsClassName,rbsScopeOperator nextgroup=rbsSuperClassOperator skipwhite
syn match rbsModuleDeclaration "[^[:space:];#<]\+"   contained contains=rbsModuleName,rbsScopeOperator
syn match rbsInterfaceDeclaration "[^[:space:];#<]\+"   contained contains=rbsInterfaceName,rbsScopeOperator

syn match rbsMethodName "\<\%([_[:alpha:]]\|[^\x00-\x7F]\)\%([_[:alnum:]]\|[^\x00-\x7F]\)*[?!=]\=\%([[:alnum:]_.:?!=]\|[^\x00-\x7F]\)\@!"            contained containedin=rbsMethodDeclaration
syn match rbsMethodName "\%(\s\|^\)\@1<=\%([_[:alpha:]]\|[^\x00-\x7F]\)\%([_[:alnum:]]\|[^\x00-\x7F]\)*[?!=]\=\%(\s\|$\)\@="               contained containedin=rbsAliasDeclaration,rbsAliasDeclaration2
syn match rbsMethodName "\%([[:space:].]\|^\)\@1<=\%(\[\]=\=\|\*\*\|[-+!~]@\=\|[*/%|&^~]\|<<\|>>\|[<>]=\=\|<=>\|===\|[=!]=\|[=!]\~\|!\|`\)\%([[:space:];#(]\|$\)\@=" contained containedin=rbsAliasDeclaration,rbsAliasDeclaration2,rbsMethodDeclaration

syn cluster rbsDeclaration contains=rbsAliasDeclaration,rbsAliasDeclaration2,rbsMethodDeclaration,rbsModuleDeclaration,rbsClassDeclaration,rbsMethodName

" Keywords {{{1
" TODO: reorganise
syn match rbsKeyword      "\%#=1\<\%(super\|yield\)\>"
syn match rbsBoolean      "\%#=1\<\%(true\|false\)\>[?!]\@!"
syn match rbsPseudoVariable "\%#=1\<\(self\|nil\)\>[?!]\@!"

syn match rbsControl "\<def\>"    nextgroup=rbsMethodDeclaration skipwhite skipnl
syn match rbsControl "\<class\>"  nextgroup=rbsClassDeclaration  skipwhite skipnl
syn match rbsControl "\<module\>" nextgroup=rbsModuleDeclaration skipwhite skipnl
syn match rbsControl "\<interface\>" nextgroup=rbsModuleDeclaration skipwhite skipnl
syn match rbsControl "\<\%(case\|begin\|do\|for\|if\|unless\|while\|until\|else\|elsif\|rescue\|ensure\|then\|when\|end\)\>"
syn match rbsKeyword "\<\%(alias\|undef\)\>"


" Special Methods {{{1
syn match rbsAccess    "\<\%(public\|protected\|private\)\>" " use re=2
syn match rbsAttribute "\%#=1\%(\%(^\|;\)\s*\)\@<=attr\>\(\s*[.=]\)\@!" " attr is a common variable name
syn match rbsAttribute "\%#=1\<attr_\%(accessor\|reader\|writer\)\>"
syn match rbsKeyword   "\%#=1\<\%(callcc\|caller\|lambda\|proc\)\>"
syn match rbsMacro   "\%#=1\<\%(include\)\>"
syn match rbsMacro   "\%#=1\<\%(alias\|define\|define_singleton\|remove\|undef\)_method\>"


" Comments and Documentation {{{1
syn match   rbsSharpBang    "\%^#!.*" display
syn keyword rbsTodo       FIXME NOTE TODO OPTIMIZE HACK REVIEW XXX todo contained
syn match   rbsEncoding     "[[:alnum:]-]\+" contained display
syn match   rbsMagicComment "\c\%<3l#\s*\zs\%(coding\|encoding\):"         contained nextgroup=rbsEncoding skipwhite
syn match   rbsMagicComment "\c\%<10l#\s*\zs\%(frozen_string_literal\|warn_indent\|warn_past_scope\):" contained nextgroup=rbsBoolean  skipwhite
syn match   rbsComment      "#.*" contains=@rbsCommentSpecial,rbsSpaceError,@Spell

syn cluster rbsCommentSpecial contains=rbsSharpBang,rbsTodo,rbsMagicComment
syn cluster rbsCommentNotTop  contains=@rbsCommentSpecial,rbsEncoding

syn region rbsDocumentation    start="^=begin\s*$"    end="^=end\s*$"              contains=rbsSpaceError,rbsTodo,@Spell

" Keyword Nobbling {{{1
" prevent methods with keyword names being highlighted as keywords when called
syn match rbsKeywordAsMethod "\%(\%(\.\@1<!\.\)\|&\.\|::\)\_s*\%([_[:lower:]][_[:alnum:]]*\|\%(BEGIN\|END\)\>\)" transparent contains=rbsDotOperator,rbsScopeOperator

" Bang and Predicate Methods and Operators {{{1
syn match rbsBangPredicateMethod "\%(\h\|[^\x00-\x7F]\)\%(\w\|[^\x00-\x7F]\)*[?!]"

" 1.9-style Hash Keys and Keyword Parameters {{{1
syn match rbsSymbol "\%(\w\|[^\x00-\x7F]\)\%(\w\|[^\x00-\x7F]\)*[?!]\=::\@!"he=e-1 contained containedin=rbsBlockParameterList,rbsCurlyBlock
syn match rbsSymbol "[]})\"':]\@1<!\<\%(\h\|[^\x00-\x7F]\)\%(\w\|[^\x00-\x7F]\)*[!?]\=:[[:space:],;]\@="he=e-1
syn match rbsSymbol "[[:space:],{(]\%(\h\|[^\x00-\x7F]\)\%(\w\|[^\x00-\x7F]\)*[!?]\=:[[:space:],;]\@="hs=s+1,he=e-1
syn match rbsSingleQuoteSymbolDelimiter "'" contained
syn match rbsSymbol "'\%(\\.\|[^']\)*'::\@!"he=e-1 contains=rbsQuoteEscape,rbsBackslashEscape,rbsSingleQuoteSymbolDelimiter
syn match rbsDoubleQuoteSymbolDelimiter "\"" contained
syn match rbsSymbol "\"\%(\\.\|[^\"]\)*\"::\@!"he=e-1 contains=@rbsStringSpecial,rbsDoubleQuoteSymbolDelimiter

" Default Highlighting {{{1
hi def link rbsDefine      Define
hi def link rbsMacro     Macro
hi def link rbsFunction    Function
hi def link rbsConditional   Conditional
hi def link rbsRepeat      Repeat
hi def link rbsControl     Statement
hi def link rbsInclude     Include
hi def link rbsInteger     Number
hi def link rbsCharacter   Character
hi def link rbsFloat     Float
hi def link rbsBoolean     Boolean
hi def link rbsException   Exception
hi def link rbsIdentifier    Identifier
hi def link rbsConstant    Type
hi def link rbsSymbol      Constant
hi def link rbsKeyword     Keyword
hi def link rbsOperator    Operator

hi def link rbsBeginEnd    Statement
hi def link rbsEval      Statement
hi def link rbsPseudoVariable    Constant
hi def link rbsCapitalizedMethod NONE

hi def link rbsComment     Comment
hi def link rbsEncoding    Constant
hi def link rbsMagicComment    SpecialComment
hi def link rbsData      Comment
hi def link rbsDataDirective   Delimiter
hi def link rbsDocumentation   Comment
hi def link rbsTodo      Todo

hi def link rbsClass     rbsDefine
hi def link rbsModule      rbsDefine
hi def link rbsExceptionHandler2 rbsDefine
hi def link rbsAccess      rbsMacro
hi def link rbsAttribute   rbsMacro
hi def link rbsMethodName    rbsFunction
hi def link rbsConditionalModifier rbsConditional
hi def link rbsExceptionHandler  rbsConditional
hi def link rbsRescueModifier    rbsExceptionHandler
hi def link rbsRepeatModifier    rbsRepeat
hi def link rbsOptionalDo    rbsRepeat
hi def link rbsClassVariable   rbsIdentifier
hi def link rbsClassName   rbsConstant
hi def link rbsModuleName    rbsConstant
hi def link rbsInterfaceName    rbsConstant
hi def link rbsGlobalVariable    rbsIdentifier
hi def link rbsInstanceVariable  rbsIdentifier
hi def link rbsPredefinedIdentifier  rbsIdentifier
hi def link rbsPredefinedConstant  rbsPredefinedIdentifier
hi def link rbsPredefinedVariable  rbsPredefinedIdentifier

hi def link rbsDefinedOperator   rbsOperator
hi def link rbsEnglishBooleanOperator  rbsOperator

hi def link rbsTernaryOperator rbsOperator
hi def link rbsArithmeticOperator  rbsOperator
hi def link rbsComparisonOperator  rbsOperator
hi def link rbsBitwiseOperator rbsOperator
hi def link rbsBooleanOperator rbsOperator
hi def link rbsRangeOperator   rbsOperator
hi def link rbsAssignmentOperator  rbsOperator
hi def link rbsEqualityOperator  rbsOperator

hi def link rbsBackslashEscape   rbsStringEscape
hi def link rbsQuoteEscape   rbsStringEscape
hi def link rbsSpaceEscape   rbsStringEscape
hi def link rbsParenthesisEscape rbsStringEscape
hi def link rbsCurlyBraceEscape  rbsStringEscape
hi def link rbsAngleBracketEscape  rbsStringEscape
hi def link rbsSquareBracketEscape rbsStringEscape
hi def link rbsStringEscape    Special

hi def link rbsInterpolationDelimiter  Delimiter
hi def link rbsSharpBang   PreProc
hi def link rbsStringDelimiter   Delimiter
hi def link rbsHeredocDelimiter  rbsStringDelimiter
hi def link rbsPercentRegexpDelimiter  rbsRegexpDelimiter
hi def link rbsPercentStringDelimiter  rbsStringDelimiter
hi def link rbsPercentSymbolDelimiter  rbsSymbolDelimiter
hi def link rbsDoubleQuoteSymbolDelimiter rbsSymbolDelimiter
hi def link rbsSingleQuoteSymbolDelimiter rbsSymbolDelimiter
hi def link rbsRegexpDelimiter   rbsStringDelimiter
hi def link rbsSymbolDelimiter   rbsSymbol
hi def link rbsString      String
hi def link rbsRegexpEscape    rbsRegexpSpecial
hi def link rbsRegexpQuantifier  rbsRegexpSpecial
hi def link rbsRegexpAnchor    rbsRegexpSpecial
hi def link rbsRegexpDot   rbsRegexpCharClass
hi def link rbsRegexpCharClass   rbsRegexpSpecial
hi def link rbsRegexpIntersection  rbsRegexpSpecial
hi def link rbsRegexpSpecial   Special
hi def link rbsRegexpComment   Comment
hi def link rbsRegexp      rbsString

hi def link rbsError     Error
hi def link rbsGlobalVariableError rbsError
hi def link rbsSpaceError    rbsError

" Postscript {{{1
let b:current_syntax = 'rbs'

let &cpoptions = s:cpo_sav
unlet! s:cpo_sav
