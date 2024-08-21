#lang s-exp syntax/module-reader

llm/lang/llm-module-lang

#:read scribble:read-inside
#:read-syntax scribble:read-syntax-inside
#:whole-body-readers? #t

(require (prefix-in scribble: scribble/reader))
