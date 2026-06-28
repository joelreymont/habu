---
title: Implement checked CASE statement
status: open
priority: 2
issue-type: task
created-at: "2026-06-28T09:27:05.007542+02:00"
---

Add a checked Forth CASE/OF/ENDOF/ENDCASE control statement. Research SwiftForth/standard Forth spelling before implementation; keep syntax idiomatic. The checker must prove every case arm and default have the same stack effect, reject missing terminators and effect-mismatched arms before runtime, and tests must cover matched case, fallthrough/default behavior, nested cases, package scope, and negative checker failures. Prefer a small factored compiler/checker implementation with docs/forth.md guidance; no untyped dispatch table fallback.
