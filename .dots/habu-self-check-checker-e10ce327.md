---
title: Self-check checker via fixpoint
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T22:34:55.593845+02:00"
---

EMIT-HOST-LOAD-PREFIX (src/habu/habu2.f:412-415) zeroes HOOK-CELL and loads util/structures/checker/render UNCHECKED (hook lands after, load row habu2.f:358). TCB = ~5900 lines of asm-emitting builder Forth + ~4400 lines checker/renderer + 226 TRUSTED: defs and ~307 TRUST rows repo-wide (91 in habu2.f, 34 in roles.f); any TRUST row typo is an unchecked soundness assumption. Fix: stage the prefix load so the previous fixpoint binary CHECKS checker.f/render.f before baking them (fixpoint infra already rebuilds bin/hb from source); machine-audit TRUST rows - generate the trusted-boundary inventory and enforce a test per row. Would have caught the sig-clobber class earlier.

---

## STOP — requires build/engine change (out of checker territory), 2026-07-03

Investigated for the type-habu wave. The staged-prefix fixpoint check lives in the
builder + fixpoint pipeline, not `src/core/checker.f`:

- `src/habu/habu2.f` loads the core/checker/render prefix via the `PFX-LOAD-ROW`
  table (~450-469) with the hook installed AFTER the prefix, so checker.f/render.f
  are baked unchecked. Staging means: before baking stage N+1's checker.f/render.f,
  run the stage-N fixpoint binary as a checker over those exact sources and fail
  the build on any reject.
- `tools/build-fixpoint.f` / `tools/build-fixpoint-main.f` own the multi-stage
  rebuild; the new "check next-stage source with current-stage binary" gate hooks
  there.
- Machine-audit of TRUST rows already exists (`tools/trusted-inventory.f` derived
  ratchet, green in this wave), so the "test per row" half is covered; the missing
  half is source-checking the prefix during bake.

The checker itself needs no change to be *run* over its own source — `CHECK!`/
`CHECK-CANDIDATE!` already type arbitrary definitions. What is missing is the build
step that FEEDS checker.f/render.f through the previous binary before baking. Owner:
the habu2.f/build worker.

## Audit refresh (2026-07-06, head 1eb3b5d3)

Count drift in the premise (tools/trusted-inventory.f is authoritative): TRUST
rows are now 356 repo-wide (101 in habu2.f, 40 habu1.f, 5 jit.f, 0 in roles.f —
the 34 roles.f rows were retired by the deftype converter rework), TRUSTED rows
235. The staged-prefix bake-check itself remains unbuilt; nonblocking
BF-CERTIFY-STAGE landed (tools/build-fixpoint.f) but does not certify the full
checker/render prefix and is not blocking.
