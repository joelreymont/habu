---
title: Default restored executable sessions to the JIT load tier
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-11T17:07:16.553782+03:00\""
---

Owner requested: Rowan tier lane; root integration. Reviewer jit_span_review on frozen f115e7885d20 saves an all-tier1 APP-IMAGE with ZCHK22; restored interactive/stdin input `ZCHK . tier@ . : POST ( -- n )33 ; POST . tier0-count@ .` returns22,1,33,0. TIER-CELL persists and cold INIT-DATA is the only reset. User requires REPL and --load to use JIT by default while saved executable bodies stay optimizing AOT. Acceptance: default fresh restored process compiler selection to0 at correct runtime initialization point, retain persisted optimized code and provenance, verify actual --load and PTY REPL, explicit set-tier1 still works. Initial finding is stdin-restored session; reviewer is checking PTY/--load before final verdict.
