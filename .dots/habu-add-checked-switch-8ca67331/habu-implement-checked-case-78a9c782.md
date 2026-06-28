---
title: Implement checked CASE statement
status: closed
priority: 2
issue-type: task
created-at: "\"2026-06-28T09:27:05.007542+02:00\""
closed-at: "2026-06-28T23:04:58.264053+02:00"
close-reason: Implemented compiler and checker support for CASE/OF/ENDOF/ENDCASE, used CASE in gate rc/outcome helpers, rebuilt bin/hb, and validated engine-suite, dictionary/checker slice, reserved-name lint, compiler-dispatch test, repair-hints test, trust-lint, typed-local-diff-lint, host-lint, filemap-lint, and full native gate (74434ms <= 90000ms).
---

Add a checked Forth CASE/OF/ENDOF/ENDCASE control statement. Research SwiftForth/standard Forth spelling before implementation; keep syntax idiomatic. The checker must prove every case arm and default have the same stack effect, reject missing terminators and effect-mismatched arms before runtime, and tests must cover matched case, fallthrough/default behavior, nested cases, package scope, and negative checker failures. Prefer a small factored compiler/checker implementation with docs/forth.md guidance; no untyped dispatch table fallback.
