" Vim syntax file
" Language:           Tiny (A PowerShell Language)
" Maintainer:         Bruce Payette
" Version:            0.1
" Project Repository: https://github.com/brucepay
" Vim Script Page:    http://www.vim.org/scripts/
"
" The following settings are available for tuning syntax highlighting:
"    let tiny_nofold_blocks = 1
"    let tiny_nofold_region = 1

" Compatible VIM syntax file start
if version < 600
	syntax clear
elseif exists("b:current_syntax")
	finish
endif

" Operators contain dashes
"setlocal iskeyword+=-

" Tiny doesn't care about case
syn case ignore

" Sync-ing method
syn sync minlines=100

" Certain tokens can't appear at the top level of the document
syn cluster tinyNotTop contains=@tinyComment,tinyCDocParam,tinyFunctionDeclaration

" Comments and special comment words
syn keyword tinyCommentTodo  TODO FIXME XXX TBD HACK NOTE BUGBUG contained
syn region tinyComment start=/\<#/ end=/#\>/ contains=tinyCommentTodo,tinyCommentDoc,@Spell
syn match tinyComment        /#.*/ contains=tinyCommentTodo,tinyCommentDoc,@Spell
syn match tinyCDocParam      /.*/ contained
syn match tinyCommentDoc     /^\s*\zs\.\w\+\>/ nextgroup=tinyCDocParam contained
syn match tinyCommentDoc     /#\s*\zs\.\w\+\>/ nextgroup=tinyCDocParam contained

" Language keywords and elements
syn keyword tinyConditional  if else elseif match
syn keyword tinyRepeat       while foreach matchlist
syn match   tinyRepeat       /\<foreach\>/ nextgroup=tinyBlock skipwhite
syn match   tinyKeyword      /\<while\>/ nextgroup=tinyBlock skipwhite

syn keyword tinyKeyword      fn def undef match matchlist while foreach break continue
syn keyword tinyKeyword      try catch finally throw return import
syn keyword tinyConstant     true false null _ Pid TinyHome __global IsLinux IsMacOS IsWindows IsCoreCLR

" Function declarations
syn keyword tinyKeyword      fn nextgroup=tinyFunctionDeclaration skipwhite
syn keyword tinyKeyword      def nextgroup=tinyFunctionDeclaration skipwhite
syn match tinyFunctionDeclaration /fn\b*\w\+\(-\w\+\)*/ contained

" Function invocations
syn match tinyFunctionInvocation /\w\+\b*(/

" Variable references
syn match tinyVariable           /\^\?\w\+/ 

" Tiny Operators
syn keyword tinyOperator is isnot as then do
syn match tinyOperator /!/
syn match tinyOperator /=/
syn match tinyOperator /+=/
syn match tinyOperator /-=/
syn match tinyOperator /\*=/
syn match tinyOperator /\/=/
syn match tinyOperator /%=/
syn match tinyOperator /+/
syn match tinyOperator /\*/
syn match tinyOperator /\*\*/
syn match tinyOperator /\//
syn match tinyOperator /|/
syn match tinyOperator /||/
syn match tinyOperator /%/
syn match tinyOperator /&&/
syn match tinyOperator /{::/
syn match tinyOperator /::}/
syn match tinyOperator /::[^}]/
syn match tinyOperator /,/
syn match tinyOperator /\./
syn match tinyOperator /\.\./
syn match tinyOperator /:+/
syn match tinyOperator /|>/
syn match tinyOperator /:>/
syn match tinyOperator /!:>/
syn match tinyOperator /<:/
syn match tinyOperator /!<:/
syn match tinyOperator /</
syn match tinyOperator /<=/
syn match tinyOperator />/
syn match tinyOperator />=/
syn match tinyOperator /==/
syn match tinyOperator /!=/
syn match tinyOperator /\.\./
syn match tinyOperator /->/
syn match tinyOperator /??/
syn match tinyOperator /?\./
syn match tinyOperator /?\[/
syn match tinyOperator /\~/
syn match tinyOperator /\*\~/
syn match tinyOperator /-\~/
syn match tinyOperator /\/\~/
syn match tinyOperator /\[/
syn match tinyOperator /\]/
syn match tinyOperator /:/
syn match tinyOperator /`[a-z0-9]\+`/

" Type literals
syn match tinyType /\[\<[a-z_][a-z0-9_.,\[\]]\+\>\]/

" Regular expressions...
syn region tinyString start=/r\// skip=/\/\// end=/\//

" Strings
syn region tinyString start=/"/ skip=/""|\\"/ end=/"/ contains=@Spell
syn region tinyString start=/'/ skip=/''/ end=/'/ contains=@Spell


" Interpolation in strings
syn match tinyEscape /`./
syn region tinyInterpolation matchgroup=tinyInterpolationDelimiter start="${" end="}" contained contains=ALLBUT,@tinyNotTop
syn region tinyNestedParentheses start="(" skip="\\\\\|\\)" matchgroup=tinyInterpolation end=")" transparent contained
syn cluster tinyStringSpecial contains=tinyEscape,tinyInterpolation,tinyVariable,tinyBoolean,tinyConstant,tinyBuiltIn,@Spell

" Numbers
syn match   tinyNumber		"\(\<\|-\)\@<=\(0[xX]\x\+\|\d\+\)\([KMGTP][B]\)\=\(\>\|-\)\@="
syn match   tinyNumber		"\(\(\<\|-\)\@<=\d\+\.\d*\|\.\d\+\)\([eE][-+]\=\d\+\)\=[dD]\="
syn match   tinyNumber		"\<\d\+[eE][-+]\=\d\+[dD]\=\>"
syn match   tinyNumber		"\<\d\+\([eE][-+]\=\d\+\)\=[dD]\>"

" Constants
syn match tinyBoolean        "\%(true\|false\)\>"
syn match tinyConstant       /\null\>/
syn match tinyBuiltIn        "$^\|$?\|$_\|$\$"
syn match tinyBuiltIn        "$\%(args\|error\|foreach\|home\|input\)\>"
syn match tinyBuiltIn        "$\%(match\(es\)\?\|myinvocation\|host\|lastexitcode\)\>"
syn match tinyBuiltIn        "$\%(ofs\|shellid\|stacktrace\)\>"

" Folding blocks
if !exists('g:tiny_nofold_blocks')
	syn region tinyBlock start=/{/ end=/}/ transparent fold
endif

if !exists('g:tiny_nofold_region')
	syn region tinyRegion start=/#region/ end=/#endregion/ transparent fold keepend extend
endif

" Setup default color highlighting
if version >= 508 || !exists("did_tiny_syn_inits")
	if version < 508
		let did_tiny_syn_inits = 1
		command -nargs=+ HiLink hi link <args>
	else
		command -nargs=+ HiLink hi def link <args>
	endif

	HiLink tinyNumber Number
	HiLink tinyBlock Block
	HiLink tinyRegion Region
	HiLink tinyException Exception
	HiLink tinyConstant Constant
	HiLink tinyString String
	HiLink tinyEscape SpecialChar
	HiLink tinyInterpolationDelimiter Delimiter
	HiLink tinyConditional Conditional
	HiLink tinyFunctionDeclaration Function
	HiLink tinyFunctionInvocation Function
	HiLink tinyVariable Identifier
	HiLink tinyBoolean Boolean
	HiLink tinyConstant Constant
	HiLink tinyBuiltIn StorageClass
	HiLink tinyType Type
	HiLink tinyComment Comment
	HiLink tinyCommentTodo Todo
	HiLink tinyCommentDoc Tag
	HiLink tinyCDocParam Todo
	HiLink tinyOperator Operator
	HiLink tinyRepeat Repeat
	HiLink tinyRepeatAndCmdlet Repeat
	HiLink tinyKeyword Keyword
	delcommand HiLink
endif

let b:current_syntax = "tinyps"
