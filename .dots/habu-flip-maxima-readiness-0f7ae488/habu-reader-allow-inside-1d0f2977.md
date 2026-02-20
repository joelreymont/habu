---
title: "Reader: allow # inside symbol tokens"
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-02-19T10:27:21.592364+01:00\\\"\""
closed-at: "2026-02-20T07:50:22.564828+01:00"
close-reason: "Lexer keeps # dispatch at token start and allows # as symbol constituent; verified via lex symbols test and maxima readiness/limit probe"
---

src/reader/lexer.zig: permit # as constituent when already inside symbol token so forms like /#alike parse (limit.lisp:901). Keep # dispatch behavior at token start unchanged. Add lexer regression test for /#alike and rerun max readiness/limit load probes.
