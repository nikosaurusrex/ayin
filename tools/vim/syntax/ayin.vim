if exists("b:current_syntax")
  finish
endif

syntax keyword ayinFunc func
syntax keyword ayinUsing using
syntax keyword ayinCast cast
syntax keyword ayinStruct struct
syntax keyword ayinEnum enum
syntax keyword ayinIn in

syntax keyword ayinIf if
syntax keyword ayinElse else
syntax keyword ayinFor for
syntax keyword ayinWhile while

syntax keyword ayinBreak break
syntax keyword ayinContinue continue

syntax keyword ayinDataType void string int float f32 f64 u8 u16 u32 u64 s8 s16 s32 s64 bool
syntax keyword ayinBool true false
syntax keyword ayinNull null

syntax keyword ayinReturn return
syntax keyword ayinDefer defer

syntax region ayinString start=/\v"/ skip=/\v\\./ end=/\v"/
syntax region ayinChar start=/\v'/ skip=/\v\\./ end=/\v'/

syntax match ayinTagNote "#\<\w\+\>" display

syntax match ayinConstant "\v<[A-Z0-9,_]+>" display

syntax match ayinInteger "\<\d\+\>" display
syntax match ayinFloat "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)\%([eE][+-]\=[0-9_]\+\)\=" display
syntax match ayinHex "\<0x[0-9A-Fa-f]\+\>" display

syntax match ayinCommentNote "@\<\w\+\>" contained display
syntax match ayinLineComment "//.*" contains=ayinCommentNote
syntax region ayinBlockComment start=/\v\/\*/ end=/\v\*\// contains=ayinBlockComment, ayinCommentNote

highlight def link ayinUsing Keyword
highlight def link ayinCast Keyword
highlight def link ayinReturn Keyword
highlight def link ayinIn Keyword
highlight def link ayinDefer Operator

highlight def link ayinBreak Keyword
highlight def link ayinContinue Keyword
highlight def link ayinFunc Keyword

highlight def link ayinString String

highlight def link ayinStruct Structure
highlight def link ayinEnum Structure

highlight def link ayinFunction Function

highlight def link ayinIf Conditional
highlight def link ayinElse Conditional
highlight def link ayinFor Repeat
highlight def link ayinWhile Repeat

highlight def link ayinLineComment Comment
highlight def link ayinBlockComment Comment
highlight def link ayinCommentNote Todo

highlight def link ayinTagNote Identifier
highlight def link ayinDataType Type
highlight def link ayinBool Boolean
highlight def link ayinConstant Constant
highlight def link ayinNull Type
highlight def link ayinInteger Number
highlight def link ayinFloat Float
highlight def link ayinHex Number

let b:current_syntax = "ayin"