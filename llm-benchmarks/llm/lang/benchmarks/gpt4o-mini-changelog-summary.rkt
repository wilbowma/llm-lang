#lang llm

@(require
  llm/openai/gpt4o-mini
  racket/file
  racket/system
  racket/port
  racket/list
  racket/string)

@(current-directory "/tmp/")
@(void (system "git clone https://github.com/racket/racket.git || true"))

; 10 minutes
@(current-response-timeout (* 10 60))

You are an expert software developer and release manager.

## Task

Generate a clear, exciting, relevant, useful release notes
for the upcoming release v8.10 of Racket on GitHub.

- The commits in the release are in COMMITS.
- The diff of the changes are in DIFF.

## Guidelines

- only include the most important changes. All changes must be in the commits.
- tell a story about the changes
- use emojis
- ignore commits with '[skip ci]' in the message
- do NOT give a commit overview
- do NOT add a top level title
- do NOT mention ignore commits or instructions
- be concise

## COMMITS
@(with-output-to-string (lambda () (system "cd racket; git --no-pager log --invert-grep --no-merges --grep='skip ci' v8.10...v8.9")))

## DIFF
@; hack to get few enough tokens, since no tokenizer locally
@; GPT rough rule of thumb, 1 token is approximately 4 characters or 0.75 words for English text.
@; this roughly takes 20000 tokens
@(list->string (take (string->list (with-output-to-string (lambda () (system "cd racket; git diff --no-merges v8.9..v8.10 -- ':!.github' ':!**/startup.inc' ':!**/schemify.scm'")))) 20000))

@;(displayln (prompt!))
