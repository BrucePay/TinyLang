" Vim syntax file
" Language:           TinyLispL (A tinyLisp Language)
" Maintainer:         Bruce Payette
" Version:            0.1
" Project Repository: https://github.com/brucepay
" Vim Script Page:    http://www.vim.org/scripts/
"
" The following settings are available for tuning syntax highlighting:
"    let tinyLisp_nofold_blocks = 1
"    let tinyLisp_nofold_region = 1

" Compatible VIM syntax file start
if version < 600
	syntax clear
elseif exists("b:current_syntax")
	finish
endif

" Operators contain dashes
"setlocal iskeyword+=-

" tinyLisp doesn't care about case
syn case ignore

" Sync-ing method
syn sync minlines=100

" Certain tokens can't appear at the top level of the document
syn cluster tinyLispNotTop contains=@tinyLispComment,tinyLispCDocParam,tinyLispFunctionDeclaration

" Comments and special comment words
syn keyword tinyLispCommentTodo  TODO FIXME XXX TBD HACK NOTE BUGBUG contained
syn match tinyLispComment        /;.*/ contains=tinyLispCommentTodo,tinyLispCommentDoc,@Spell

" Language keywords and elements
syn keyword tinyLispKeyword      if while foreach setq let cond do pipe define sform defmacro time call lambda quote and or

syn keyword tinyLispKeyword      try catch finally throw return import
syn keyword tinyLispConstant     true false nil _ Pid  IsLinux IsMacOS IsWindows IsCoreCLR IsUnix


" Variable references
syn match tinyLispVariable           /\w\+/ 

" tinyLisp Operators
syn keyword tinyLispOperator is isnot as then do
syn match tinyLispOperator /!/
syn match tinyLispOperator /=/
syn match tinyLispOperator /+=/
syn match tinyLispOperator /-=/
syn match tinyLispOperator /\*=/
syn match tinyLispOperator /\/=/
syn match tinyLispOperator /%=/
syn match tinyLispOperator /+/
syn match tinyLispOperator /\*/
syn match tinyLispOperator /\*\*/
syn match tinyLispOperator /\//
syn match tinyLispOperator /|/
syn match tinyLispOperator /||/
syn match tinyLispOperator /%/
syn match tinyLispOperator /&&/
syn match tinyLispOperator /{::/
syn match tinyLispOperator /::}/
syn match tinyLispOperator /::[^}]/
syn match tinyLispOperator /,/
syn match tinyLispOperator /\./
syn match tinyLispOperator /\.\./
syn match tinyLispOperator /:+/
syn match tinyLispOperator /|>/
syn match tinyLispOperator /:>/
syn match tinyLispOperator /!:>/
syn match tinyLispOperator /<:/
syn match tinyLispOperator /!<:/
syn match tinyLispOperator /</
syn match tinyLispOperator /<=/
syn match tinyLispOperator />/
syn match tinyLispOperator />=/
syn match tinyLispOperator /==/
syn match tinyLispOperator /!=/
syn match tinyLispOperator /\.\./
syn match tinyLispOperator /->/
syn match tinyLispOperator /??/
syn match tinyLispOperator /?\./
syn match tinyLispOperator /?\[/
syn match tinyLispOperator /\~/
syn match tinyLispOperator /\*\~/
syn match tinyLispOperator /-\~/
syn match tinyLispOperator /\/\~/
syn match tinyLispOperator /\[/
syn match tinyLispOperator /\]/
syn match tinyLispOperator /:/
syn match tinyLispOperator /`[a-z0-9]\+`/
syn match tinyLispOperator /'[^ ()]+/

" Type literals
syn match tinyLispType /\[[a-z_][a-z0-9_.,\[\]]\+\]/

" Regular expressions...
syn region tinyLispString start=/r\// skip=/\/\// end=/\//

" Strings
syn region tinyLispString start=/"/ skip=/""|\\"/ end=/"/ contains=@Spell


" Interpolation in strings
syn region tinyLispInterpolation matchgroup=tinyLispInterpolationDelimiter start="${" end="}" contained contains=ALLBUT,@tinyLispNotTop
syn region tinyLispNestedParentheses start="(" skip="\\\\\|\\)" matchgroup=tinyLispInterpolation end=")" transparent contained
syn cluster tinyLispStringSpecial contains=tinyLispEscape,tinyLispInterpolation,tinyLispVariable,tinyLispBoolean,tinyLispConstant,tinyLispBuiltIn,@Spell

" Numbers
syn match   tinyLispNumber		"\(\<\|-\)\@<=\(0[xX]\x\+\|\d\+\)\([KMGTP][B]\)\=\(\>\|-\)\@="
syn match   tinyLispNumber		"\(\(\<\|-\)\@<=\d\+\.\d*\|\.\d\+\)\([eE][-+]\=\d\+\)\=[dD]\="
syn match   tinyLispNumber		"\<\d\+[eE][-+]\=\d\+[dD]\=\>"
syn match   tinyLispNumber		"\<\d\+\([eE][-+]\=\d\+\)\=[dD]\>"

" Constants
syn match tinyLispBoolean        "\%(true\|false|t\)\>"
syn match tinyLispConstant       /\nil\>/

" Folding blocks
if !exists('g:tinyLisp_nofold_blocks')
	syn region tinyLispBlock start=/(/ end=/)/ transparent fold
endif

if !exists('g:tinyLisp_nofold_region')
	syn region tinyLispRegion start=/;region/ end=/;endregion/ transparent fold keepend extend
endif

" Setup default color highlighting
if version >= 508 || !exists("did_tinyLisp_syn_inits")
	if version < 508
		let did_tinyLisp_syn_inits = 1
		command -nargs=+ HiLink hi link <args>
	else
		command -nargs=+ HiLink hi def link <args>
	endif

	HiLink tinyLispNumber Number
	HiLink tinyLispBlock Block
	HiLink tinyLispRegion Region
	HiLink tinyLispException Exception
	HiLink tinyLispConstant Constant
	HiLink tinyLispString String
	HiLink tinyLispEscape SpecialChar
	HiLink tinyLispInterpolationDelimiter Delimiter
	HiLink tinyLispConditional Conditional
	HiLink tinyLispFunctionDeclaration Function
	HiLink tinyLispFunctionInvocation Function
	HiLink tinyLispVariable Identifier
	HiLink tinyLispBoolean Boolean
	HiLink tinyLispConstant Constant
	HiLink tinyLispBuiltIn StorageClass
	HiLink tinyLispType Type
	HiLink tinyLispComment Comment
	HiLink tinyLispCommentTodo Todo
	HiLink tinyLispCommentDoc Tag
	HiLink tinyLispCDocParam Todo
	HiLink tinyLispOperator Operator
	HiLink tinyLispRepeat Repeat
	HiLink tinyLispRepeatAndCmdlet Repeat
	HiLink tinyLispKeyword Keyword
	delcommand HiLink
endif

let b:current_syntax = "tinyLisplisp"
