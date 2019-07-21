" syntax highlighting for purescript
"
" Heavily modified version of the purescript syntax
" highlighter to support purescript.
"
" author: raichoo (raichoo@googlemail.com)

if exists("b:current_syntax")
  finish
endif

" Values:
syn match purescriptIdentifier "\<[_a-z]\(\w\|\'\)*\>"
syn match purescriptNumber "0[xX][0-9a-fA-F]\+\|0[oO][0-7]\|[0-9]\+"
syn match purescriptFloat "[0-9]\+\.[0-9]\+\([eE][-+]\=[0-9]\+\)\="
syn keyword purescriptBoolean true false

" Delimiters:
syn match purescriptDelimiter "[,;|.()[\]{}]"

" Type:
" syn match purescriptType "\%(\<class\s\+\)\@15<!\<\u\w*\>" contained
"   \ containedin=purescriptTypeAlias
"   \ nextgroup=purescriptType,purescriptTypeVar skipwhite
" Note: changed this! from the above
syn match purescriptType "\<[_A-Z]\(\w\|\'\)*\>" contained
      \ containedin=purescriptData,purescriptNewtype,purescriptTypeAlias,purescriptFunctionDecl
syn match purescriptTypeVar "\<[_a-z]\(\w\|\'\)*\>" contained
  \ containedin=purescriptClassDecl,purescriptData,purescriptNewtype,purescriptTypeAlias,purescriptFunctionDecl
syn region purescriptTypeExport matchgroup=purescriptType start="\<[A-Z]\(\S\&[^,.]\)*\>("rs=e-1 matchgroup=purescriptDelimiter end=")" contained extend
  \ contains=hsArrow,hsInteger,purescriptConstructor,purescriptDelimiter

" Constructor:
syn match purescriptConstructor "\%(\<class\s\+\)\@15<!\<\u\w*\>"
syn region purescriptConstructorDecl matchgroup=purescriptConstructor start="\<[A-Z]\w*\>" end="\(|\|$\)"me=e-1,re=e-1 contained
  \ containedin=purescriptData,purescriptNewtype
  \ contains=purescriptType,purescriptTypeVar,purescriptDelimiter,purescriptOperatorType,purescriptOperatorTypeSig,@purescriptComment


" Function:
syn match purescriptFunction "\%(\<instance\s\+\|\<class\s\+\)\@18<!\<[_a-z]\(\w\|\'\)*\>" contained
" syn match purescriptFunction "\<[_a-z]\(\w\|\'\)*\>" contained
syn match purescriptFunction "(\%(\<class\s\+\)\@18<!\(\W\&[^(),\"]\)\+)" contained extend
syn match purescriptBacktick "`[_A-Za-z][A-Za-z0-9_\.]*`"

" Class:
syn region purescriptClassDecl start="^\%(\s*\)class\>"ms=e-5 end="\<where\>\|$"
  \ contains=hsArrow,hsTypeColon,hsConstraintArrow,purescriptClass,purescriptClassName,purescriptDelimiter,purescriptOperatorTypeSig,purescriptOperatorType,purescriptOperator,purescriptType,purescriptWhere
  \ nextgroup=purescriptClass
  \ skipnl
syn match purescriptClass "\<class\>" containedin=purescriptClassDecl contained
  \ nextgroup=purescriptClassName
  \ skipnl
syn match purescriptClassName "\<[A-Z]\w*\>" containedin=purescriptClassDecl contained

" Module:
syn match purescriptModuleName "\(\u\w\*\.\?\)*" contained excludenl
syn match purescriptModuleKeyword "\<module\>"
syn match purescriptModule "^module\>\s\+\<\(\w\+\.\?\)*\>"
  \ contains=purescriptModuleKeyword,purescriptModuleName
  \ nextgroup=purescriptModuleParams
  \ skipwhite
  \ skipnl
  \ skipempty
syn region purescriptModuleParams start="(" skip="([^)]\{-})" end=")" fold contained keepend
  \ contains=purescriptClassDecl,purescriptClass,purescriptClassName,purescriptDelimiter,purescriptType,purescriptTypeExport,purescriptStructure,purescriptModuleKeyword,@purescriptComment
  \ nextgroup=purescriptImportParams skipwhite

" Import:
syn match purescriptImportKeyword "\<\(foreign\|import\|qualified\)\>"
syn match purescriptImport "\<import\>\s\+\(qualified\s\+\)\?\<\(\w\+\.\?\)*"
  \ contains=purescriptImportKeyword,purescriptModuleName
  \ nextgroup=purescriptImportParams,purescriptImportAs,purescriptImportHiding
  \ skipwhite
syn region purescriptImportParams
  \ start="("
  \ skip="([^)]\{-})"
  \ end=")"
  \ contained
  \ contains=purescriptClass,purescriptClass,purescriptStructure,purescriptType,purescriptIdentifier
  \ nextgroup=purescriptImportAs
  \ skipwhite
syn keyword purescriptAsKeyword as contained
syn match purescriptImportAs "\<as\>\_s\+\u\w*"
  \ contains=purescriptAsKeyword,purescriptModuleName
  \ nextgroup=purescriptModuleName
syn keyword purescriptHidingKeyword hiding contained
syn match purescriptImportHiding "hiding"
  \ contained
  \ contains=purescriptHidingKeyword
  \ nextgroup=purescriptImportParams
  \ skipwhite

" Function declaration:
syn region purescriptFunctionDecl
  \ excludenl start="^\z(\s*\)\(\(foreign\s\+import\)\_s\+\)\?[_a-z]\(\w\|\'\)*\_s\{-}\(::\|∷\)"
  \ end="^\z1\=\S"me=s-1,re=s-1 keepend
  \ contains=hsInteger,hsForall,hsArrow,hsConstraintArrow,hsTypeColon,purescriptFunctionDeclStart,purescriptForall,purescriptOperatorType,purescriptOperatorTypeSig,purescriptType,purescriptTypeVar,purescriptDelimiter,@purescriptComment
syn region purescriptFunctionDecl
  \ excludenl start="^\z(\s*\)where\z(\s\+\)[_a-z]\(\w\|\'\)*\_s\{-}\(::\|∷\)"
  \ end="^\(\z1\s\{5}\z2\)\=\S"me=s-1,re=s-1 keepend
  \ contains=purescriptFunctionDeclStart,purescriptForall,purescriptOperatorType,purescriptOperatorTypeSig,purescriptType,purescriptTypeVar,purescriptDelimiter,@purescriptComment
syn region purescriptFunctionDecl
  \ excludenl start="^\z(\s*\)let\z(\s\+\)[_a-z]\(\w\|\'\)*\_s\{-}\(::\|∷\)"
  \ end="^\(\z1\s\{3}\z2\)\=\S"me=s-1,re=s-1 keepend
  \ contains=hsInteger,hsArrow,purescriptFunctionDeclStart,purescriptForall,purescriptOperatorType,purescriptOperatorTypeSig,purescriptType,purescriptTypeVar,purescriptDelimiter,@purescriptComment
syn match purescriptFunctionDeclStart "^\s*\(\(foreign\s\+import\|let\|where\)\_s\+\)\?\([_a-z]\(\w\|\'\)*\)\_s\{-}\(::\|∷\)" contained
  \ contains=hsInteger,hsTypeColon,hsArrow,purescriptImportKeyword,purescriptWhere,purescriptLet,purescriptFunction,purescriptOperatorType
" syn keyword purescriptForall forall
" syn match purescriptForall "∀"

" Keywords:
syn keyword purescriptConditional if then else
syn keyword purescriptStatement do case of in ado
syn keyword purescriptLet let
syn keyword purescriptWhere where
syn match purescriptStructure "\<\(data\|newtype\|type\|kind\)\>"
  \ nextgroup=purescriptType skipwhite
syn keyword purescriptStructure derive
syn keyword purescriptStructure instance
  \ nextgroup=purescriptFunction skipwhite

" Highlight the FnWireframe navigation spots
syn keyword fnWireframe where do let case if
" Infix
syn match purescriptInfixKeyword "\<\(infix\|infixl\|infixr\)\>"
syn match purescriptInfix "^\(infix\|infixl\|infixr\)\>\s\+\([0-9]\+\)\s\+\(type\s\+\)\?\(\S\+\)\s\+as\>"
  \ contains=purescriptInfixKeyword,purescriptNumber,purescriptAsKeyword,purescriptConstructor,purescriptStructure,purescriptFunction,purescriptBlockComment
  \ nextgroup=purescriptFunction,purescriptOperator,@purescriptComment

" Operators:
syn match purescriptOperator "\([-!#$%&\*\+/<=>\?@\\^|~:]\|\<_\>\)"
syn match purescriptOperatorType "\%(\<instance\>.*\)\@40<!\(::\|∷\)"
  \ nextgroup=purescriptForall,purescriptType skipwhite skipnl skipempty
syn match purescriptOperatorFunction "\(->\|<-\|[\\→←]\)"
syn match purescriptOperatorTypeSig "\(->\|<-\|=>\|<=\|::\|[∷∀→←⇒⇐]\)" contained
  \ nextgroup=purescriptType skipwhite skipnl skipempty

" Type definition:
syn region purescriptData start="^data\s\+\([A-Z]\w*\)" end="^\S"me=s-1,re=s-1 transparent
syn match purescriptDataStart "^data\s\+\([A-Z]\w*\)" contained
  \ containedin=purescriptData
  \ contains=purescriptStructure,purescriptType,purescriptTypeVar
syn match purescriptForeignData "\<foreign\s\+import\s\+data\>"
  \ contains=purescriptImportKeyword,purescriptStructure
  \ nextgroup=purescriptType skipwhite

syn region purescriptNewtype start="^newtype\s\+\([A-Z]\w*\)" end="^\S"me=s-1,re=s-1 transparent
syn match purescriptNewtypeStart "^newtype\s\+\([A-Z]\w*\)" contained
  \ containedin=purescriptNewtype
  \ contains=purescriptStructure,purescriptType,purescriptTypeVar

syn region purescriptTypeAlias start="^type\s\+\([A-Z]\w*\)" end="^\S"me=s-1,re=s-1 transparent
syn match purescriptTypeAliasStart "^type\s\+\([A-Z]\w*\)" contained
  \ containedin=purescriptTypeAlias
  \ contains=purescriptStructure,purescriptType,purescriptTypeVar

" String:
syn match purescriptChar "'[^'\\]'\|'\\.'\|'\\u[0-9a-fA-F]\{4}'"
syn region purescriptString start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=@Spell
syn region purescriptMultilineString start=+"""+ end=+"""+ fold contains=@Spell

" Comment:
syn match purescriptLineComment "---*\([^-!#$%&\*\+./<=>\?@\\^|~].*\)\?$" contains=@Spell,m4,mbr1
syn region purescriptBlockComment start="{-" end="-}" fold
  \ contains=purescriptBlockComment,@Spell
syn cluster purescriptComment contains=purescriptLineComment,purescriptBlockComment,@Spell

syn sync minlines=50


" ─   Conceal with unicode                               ■

" This replaces the following insert maps
" inoremap :: <c-k>::
" inoremap -> <c-k>->
" inoremap <- <c-k><-
" inoremap => <c-k>=>
" inoremap <= <c-k><=
" inoremap forall <c-k>FA

" Note: These IDs need to be included in the 'contains=' section of e.g. 'purescriptFunctionDecl', etc

syntax match hsArrow                '\s\zs->' conceal cchar=→
syntax match hsArrowBackw           '\s\zs<-' conceal cchar=←
syntax match hsConstraintArrow      '\s\zs=>' conceal cchar=⇒
syntax match hsConstraintArrowBackw '\s\zs<=' conceal cchar=⇐
syntax match hsTypeColon            '::' conceal cchar=∷
syntax match hsForall               '\s\zsforall' conceal cchar=∀

" Tests:
" zipThese :: forall a b. Num a => [a] -> [b] -> [These a b]
" class Functor f => Align (f :: * -> *) where

" Show lambda and conceal unicode characters
" Issue Question: The highlight group does not seem to have an effect here - the Conceal group is used. Would ideally
" like to color the conceal character differently.
syntax match Normal '\\\%([^\\]\+→\)\@=' conceal cchar=λ
syntax match Normal ' \zs\.' conceal cchar=∘
syntax match Normal ' \zs<\$>' conceal cchar=⫩
syntax match Normal ' \zs<\*>' conceal cchar=⟐
syntax match Normal ' \zs>>' conceal cchar=≫
syntax match Normal ' \zs>>=' conceal cchar=⫦
syntax match Normal ' \zs\`elem\`' conceal cchar=∈
syntax match Normal ' \zs\`flipElem\`' conceal cchar=∋
syntax match Normal ' \zs>=>' conceal cchar=↣
syntax match Normal ' \zs<=<' conceal cchar=↢
syntax match Normal ' \zs==' conceal cchar=≡
syntax match Normal ' \zs<>' conceal cchar=◇
syntax match Normal ' \zsmempty' conceal cchar=∅
syntax match Normal ' \zs++' conceal cchar=⧺
syntax match Normal ' \zs<=' conceal cchar=≤
syntax match Normal ' \zs>=' conceal cchar=≥
syntax match hsInteger 'Integer' conceal cchar=ℤ
" Note: How to set up conceal: the following line has the same effect as the line above. The highlightgroup for syntax
" match seems to have no effect - still uses the Operator highlight?. For matchadd the 'Conceal' group is mandatory.
" call matchadd('Conceal', ' \zs\.', -1, -1, {'conceal': '∘'})


" ─^  Conceal with unicode                               ▲

" ─   Inline Tests conceals                              ■

" Example data declaration 1-9: hides the first identifier (the function to be tested)
" e1_database4 = database4 (Just "eins") 123
" -- ①  (Just eins) 123
syntax match InlineTestNum   'e1' conceal cchar=①
syntax match InlineTestNum   'e2' conceal cchar=②
syntax match InlineTestNum   'e3' conceal cchar=③
syntax match InlineTestNum   'e4' conceal cchar=④
syntax match InlineTestNum   'e5' conceal cchar=⑤
syntax match InlineTestNum   'e6' conceal cchar=⑥
syntax match InlineTestIdeSpace '\v_\i+\ze[\)\ ]' contained conceal cchar= 
" Notes: The '+' is needed to prevent concealing standalone '_'s
" syntax match InlineTestDecSpace '\v_\i{-}\s\=\ze\s' conceal cchar= 
syntax match InlineTestDecSpace '\v_\i{-}\s\=\ze\s' contained conceal cchar= 

syntax match InlineTestIdentifier  '\ve\d_\i{-}\ze[\)| ]' contains=InlineTestNum,InlineTestIdeSpace
" syntax match InlineTestDeclaration '\v^e\d_\i{-}\s\=\s\i{-}\s' contains=InlineTestNum,InlineTestDecSpace
syntax match InlineTestDeclaration '\v^e\d_\i{-}\s\=\s' contains=InlineTestNum,InlineTestDecSpace

" TODO this is sort of benefitial (but coincidential and i could not set up a separate HL-group for this when tried in
" 7-2019:
" a12_fcomposedNum = e1_fcomposedNum == e3_composedNum
" only "'e3_' gets concealed, but when i append a <space> the entire symbol gets concealed
"

" Assertions:
" a15__database3 = (snd <$> e1_database4) `flipElem` 123
" └ (snd <$> ① ) ∋ 123
syntax match InlineTestAssertDec     '\v^a\d\d_\i{-}\s\=\ze\s' conceal cchar=├
syntax match InlineTestAssertLastDec '\v^a\d\d__\i{-}\s\=\ze\s' conceal cchar=└
syntax match InlineTestAssertDecAndTestIdentif  '\v^a\d\d_\i{-}\s\=\se\d\i{-}\ze\s' conceal cchar=├

" syntax match Normal '\v^e1_\S{-}\s\=\s\S{-}\ze\s' conceal cchar=①
" call matchadd('Conceal', '\v^txe10\s\=\s.{-}\ze\s', 12, -1, {'conceal': '①'})
" call matchadd('Conceal', 'txe10\s', 12, -1, {'conceal': '①'})
" -- could do this in a for loop to allow 9 example data sets:
" call matchadd('Conceal', '\v^txe20\s\=\s.{-}\ze\s', 12, -1, {'conceal': '②'})

" Assertions 1-9:
" call matchadd('Conceal', '\vtxa\d\d\s\=', 12, -1, {'conceal': '├'})
" -- if datasource is first symbol, conceal it
" call matchadd('Conceal', '\vtxa\d\d\s\=\stxe\d\d\s', 12, -1, {'conceal': '├'})

" ─^  Inline Tests conceals                              ▲


" Tools:
" get syntax group
" echo synIDattr( synID( line('.'), col('.'), 0), 'name' )

" ─   Markbar                                            ■

" This will highlight only the "D" ID in this line:
" [D] file Name
" syn region markbarHeader start=/^\[/ end=/$/ contains=markbarID
syn region markbarHeader start=/^|/ end=/$/ contains=markbarID
syn region markbarID start=/|/hs=s+1 end=/|/he=e-1 contained
" - "contained" marks that this definition is only used in other groups
"   which explicitly reference it in "contains=<groupid>"
"   It does not match on the root level/ by itself

hi! def link markbarHeader Keyword
hi! def link markbarID Highlight1

" highlight vimscript comments. avoids triggring a multiline purescript string
" with prefixed whitespace
" Issue: Repl output String types are highlighted as vim-comments → temp disable this
" syn region markbarVimscriptComment1 start=/^\s\+\"\s/ end=/$/
" syn region markbarVimscriptComment start=/^\"/ end=/$/
" hi! def link markbarVimscriptComment Comment
" hi! def link markbarVimscriptComment1 Comment

" highlight markdown comment
syn region markbarMarkdownHeader start=/^\#\+\s/ end=/$/
syn region markbarMarkdownComment start=/^>/ end=/$/
hi! def link markbarMarkdownHeader Title
hi! def link markbarMarkdownComment Comment

" ─^  Markbar                                            ▲


" highlight links
highlight def link purescriptModule Include
highlight def link purescriptImport Include
highlight def link purescriptModuleKeyword purescriptKeyword
highlight def link purescriptImportAs Include
highlight def link purescriptModuleName Include
highlight def link purescriptModuleParams purescriptDelimiter
highlight def link purescriptImportKeyword purescriptKeyword
highlight def link purescriptAsKeyword purescriptKeyword
highlight def link purescriptHidingKeyword purescriptKeyword

highlight def link purescriptConditional Conditional
highlight def link purescriptWhere purescriptKeyword
highlight def link purescriptInfixKeyword purescriptKeyword

highlight def link purescriptBoolean Boolean
highlight def link purescriptNumber Number
highlight def link purescriptFloat Float

highlight def link purescriptDelimiter Delimiter

highlight def link purescriptOperatorTypeSig purescriptOperatorType
highlight def link purescriptOperatorFunction purescriptOperatorType
highlight def link purescriptOperatorType purescriptOperator

highlight def link purescriptConstructorDecl purescriptConstructor
highlight def link purescriptConstructor purescriptFunction

highlight def link purescriptTypeVar Identifier
highlight def link purescriptForall purescriptStatement

highlight def link purescriptChar String
highlight def link purescriptBacktick purescriptOperator
highlight def link purescriptString String
highlight def link purescriptMultilineString String

highlight def link purescriptLineComment purescriptComment
highlight def link purescriptBlockComment purescriptComment

" purescript general highlights
highlight def link purescriptClass purescriptKeyword
highlight def link purescriptClassName Type
highlight def link purescriptStructure purescriptKeyword
highlight def link purescriptKeyword Keyword
highlight def link purescriptStatement Statement
highlight def link purescriptLet Statement
highlight def link purescriptOperator Operator
highlight def link purescriptFunction Function
highlight def link purescriptType Type
highlight def link purescriptComment Comment

let b:current_syntax = "purescript"
