---
title: Default restored executable sessions to the JIT load tier
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-09-11T17:07:16.553782+03:00\\\"\""
closed-at: "2026-09-16T14:34:48.015653+03:00"
close-reason: "superseded by habu-campaign-c1-finish-1f129a00: Restored executable sessions must default to the JIT tier while saved bodies stay optimizing AOT; residue is the reset point after every fresh-process restore."
---

Owner requested: Rowan tier lane; root integration. Reviewer jit_span_review on frozen f115e7885d20 saves an all-tier1 APP-IMAGE with ZCHK22; restored interactive/stdin input `ZCHK . tier@ . : POST ( -- n )33 ; POST . tier0-count@ .` returns22,1,33,0. TIER-CELL persists and cold INIT-DATA is the only reset. User requires REPL and --load to use JIT by default while saved executable bodies stay optimizing AOT. Acceptance: default fresh restored process compiler selection to0 at correct runtime initialization point, retain persisted optimized code and provenance, verify actual --load and PTY REPL, explicit set-tier1 still works. Initial finding is stdin-restored session; reviewer is checking PTY/--load before final verdict.


Update2026-09-11 14:15 UTC: Owner confirmed Rowan. Reset selected tier to0 after all fresh-process restore paths, leaving persisted optimized bodies intact; actual --load and PTY acceptance requested. Independent jit_span_review final verdict NOT CLEAR for this and active-definition consistency. Original interval, rejected-emission, alias/defer/CP and capacity/save refusal probes passed on frozen f115e788.
