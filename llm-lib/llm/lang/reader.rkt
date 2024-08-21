#lang s-exp syntax/module-reader

llm/lang/module-lang

#:read scribble:read-inside
#:read-syntax scribble:read-syntax-inside
#:whole-body-readers? #t

(require (prefix-in scribble: scribble/reader))
