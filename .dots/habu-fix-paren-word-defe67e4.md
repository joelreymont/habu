---
title: Fix paren-word lexing in lint lexer
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T17:23:26.146202+02:00"
---

Full context: tools/lint/source-lex.f:466 starts a paren comment at ANY token whose first byte is ( — but standard Forth only opens a comment at a standalone ( delimited by whitespace, so a word legitimately named (CMP) (defined at src/habu/habu1.f:1332) lexes as a COMMENT token. tools/package-diff-lint-core.f's definition scan (OLD-START-DEFINITION, ~line 927) then sees the : definer with no name token and throws E-DIFF-SYNTAX, which misattributes a source-shape defect to the diff artifact — the file's own error-block comment (-4803..-4806) forbids exactly that. Effect: any commit whose diff touches src/habu/habu1.f cannot pass the package commit gate; the raw-storage seal commit is blocked on this. Fix: (1) in the lexer, open a paren comment only when the ( is followed by whitespace or end of input, else scan the token as a WORD; (2) route the no-name-definer defect to a new named code (e.g. -4807 E-PKGDIFF-NONAME) beside the existing source-defect block; (3) negative fixtures: a word named (CMP) in definition-name position, in call position, a real ( comment ), and a definer at end of input with no name — each proving the specific rejection or acceptance. Acceptance: package-diff-lint runs clean past habu1.f's (CMP); the fixtures fail on the pre-fix lexer; typed-local-diff-lint and primitive-effect-inventory (same lexer) stay green.
