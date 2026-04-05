---
title: Split load APIs
status: open
priority: 1
issue-type: task
created-at: "2026-04-05T11:37:31.436855+02:00"
blocks:
  - habu-canonicalize-maxima-root-32d71ea3
---

Problem: PLAN.md 1.4c still needs authoritative fail-closed load/eval APIs separated from diagnostic enumeration helpers; current helper surface can still mix authoritative execution and exploratory listing. Acceptance: one authoritative load path fails closed, optional diagnostics are non-authoritative, and loader/runner/bench/tool code use the same canonical entrypoint. Files: PLAN.md:436-456, src/interp/repl.zig, src/main.zig, tools/maxima-rtest.lisp, bench/maxima_workload.zig. Verify: authoritative paths abort on any lookup/load failure while diagnostic paths cannot be mistaken for success.
