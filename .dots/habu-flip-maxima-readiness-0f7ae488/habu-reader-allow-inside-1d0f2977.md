---
title: "Reader: allow # inside symbol tokens"
status: active
priority: 2
issue-type: task
created-at: "\"2026-02-19T10:27:21.592364+01:00\""
---

src/reader/lexer.zig: permit # as constituent when already inside symbol token so forms like /#alike parse (limit.lisp:901). Keep # dispatch behavior at token start unchanged. Add lexer regression test for /#alike and rerun max readiness/limit load probes.
