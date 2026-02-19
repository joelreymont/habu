---
title: Reader/parser parity
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-17T22:23:04.168525+01:00\""
closed-at: "2026-02-19T23:51:06.067163+01:00"
close-reason: "covered by #./read-suppress and array-terminal regressions (src/tests/integration.zig)"
blocks:
  - habu-define-maxima-gates-aca4e665
---

src/reader/lexer.zig and src/reader/parser.zig. Cause: reader edge cases stop Maxima forms before compiler. Fix: close #. and array terminal parsing gaps with tests.
