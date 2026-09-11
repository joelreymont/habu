---
title: Separate multiline definitions
status: open
priority: 3
issue-type: task
created-at: "2026-07-21T22:40:04.116483+02:00"
---

Invariant: a Forth definition that occupies more than one physical line is followed by one blank physical line before the next definition or declaration; consecutive genuinely one-line definitions may remain grouped. Current production and test blocks run multiline definitions together, obscuring where one unit ends and another begins. A broad lexical scan finds 4,718 immediate terminator-to-definition adjacencies across 544 files, but that is only a candidate upper bound because one-line definitions are valid.

Specify the rule in the Forth standard, build a tokenizer-aware checked census that tracks real definition openers and terminators, classify the candidates, and clean every actual violation globally. The scanner must understand colon, CHECKED, TRUSTED, KERNEL, declarers, quotations, nested compile constructs, comments, and strings rather than matching raw semicolons. It must preserve grouped one-line tables and must not insert whitespace inside generated source payloads unless that payload is separately validated source.

Add positive and negative fixtures for multiline and one-line groups, comments, strings, quotations, declarers, and end-of-file. Run every touched exact load, source-layout lint, typed-local and package lints, bootstrap, fixpoint, Maki, PTX standard library, and full native gates. Compare source tokens, emitted bytes, JIT, DATA, and CODELEN before and after; the change must be semantically and runtime-byte neutral.
