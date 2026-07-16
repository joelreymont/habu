---
title: "Core records: remove boot DSL"
status: active
priority: 1
issue-type: task
created-at: "2026-07-13T16:46:05.263733+02:00"
blocks:
  - habu-core-records-install-cf779d06
---

Remove the pre-checker record DSL from checker, type-schema, and type-family.
Replace each private implementation record with named cell/byte offsets, a
named stride, ordinary accessors, and load-time offset/size/alignment/pointer
role assertions. Preserve arena, cache, snapshot, diagnostic, and recovery ABIs
exactly. Establish identical native/recovery layouts and move the sole public
STRUCTURE/ENUM parser after render and check-hook. No pre-checker family ids,
reflection, constructors, parser, definer, descriptor arena, adoption phase,
snapshot rows, or AOT rows survive. Run exact core loads, engine suites,
source certification, typed-local diff lint, trust lint, and fixpoint gate.

Claim: agent=sol workspace=.jj-ws/habu-core-records-close
