---
title: Add WSTORE scoped read over held resident
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T17:11:32.436417+02:00"
blocks:
  - habu-delete-resident-and-05c594cb
---

OBSOLETE - DO NOT IMPLEMENT (2026-07-27, codex critical-path correction, ruled
by the orchestrator). The interface described below is scheduled for deletion,
so building it would manufacture an API that a queued leaf then removes. Two
independent reasons, both measured rather than argued:

First, the embedded-store chain retires the premise. `habu-embed-store-in-f8109695`
already proves `gpt2-model` can own `WSTORE:store` directly, and its leaf
`habu-delete-resident-and-05c594cb` explicitly deletes `WSTORE:resident`, `HOLD`,
`RESIDENT-DISPOSE`, the park and unpark trusted erasures, and the pre-reserved
HOLD cells - which is every part this contract's `WITH-RESIDENT-SLOT` was built
on top of. That delete leaf already names closing this dot as part of its own
work. It is recorded above as the blocker so this dot cannot be dispatched
ahead of the deletion that supersedes it.

Second, the slot scope strands its owner on a throwing user body: the checked
probe proved the callback CAN carry caller state on the ambient return row (an
earlier cannot-thread-state rationale was falsified by that probe and is
withdrawn), but a body that throws leaves the held store stranded, and the
first post-cutover weight consumer is a model-owned total EMBED operation, not
a new resident API.

The claim is released and no partial work lands: the worker was stopped with an
uncommitted partial diff only. Status stays open rather than closed because the
delete leaf owns the closure, and closing it here would take that step out of
the leaf that has to prove the sweep. What replaces the capability is the
model-owned row-polymorphic weight loan, scheduled after the resident deletion.

The original contract is preserved below as history. Read it as the record of a
design that was overtaken, not as work to pick up.

Why: both slot tables inside a committed GPT-2 model are currently write-only.
The forward pass will trust those rows for every weight, so a wrong offset can
return plausible bytes without any production-path test observing the defect.

Owner and interface: package `WSTORE` owns
`WITH-RESIDENT-SLOT
( WSTORE:resident n [ ptr u8 n -- n ] -- WSTORE:resident n )`.
It lends exactly one validated slot span and returns the same held resident plus
the body result. This dot blocks `habu-bind-txn-bind-d402a260`.

Dependencies: the WSTORE builder and disposal contracts
`habu-add-wstore-builder-606aaa1c` and
`habu-add-wstore-public-db6c70fe`, plus the total SAFET mapping contract, which
has since landed and closed - it was `habu-return-typed-mem-ac35e3c9`, delivered
as e0b22bf2 "Make SAFET mapping detach total" and fa96f47f "Make mapping scope
total", both ancestors of master.

Design: when `HOLD` still owns the mapped or allocated arm, cache that arm's
immutable base pointer and full byte length in the resident table header. Keep
the real arm parked solely for final disposal. Before calling the user body,
convert the public resident to one package-private linear guard. Only that guard
may remain below `ptr u8 n`; the body must be unable to name, reconstruct, or
dispose a public `WSTORE:resident`, `WSTORE:buffer`, or `SAFET:mapping`. On
normal return, convert the guard back to the resident. On any slot, extent, or
body failure, consume the guard and dispose the parked owner exactly once, then
rethrow; cleanup failure takes precedence over the original failure.

Forbidden: public raw accessors, representation readers, duplicate owners,
owner reconstruction while the body runs, a second slot API, sentinels,
compatibility paths, runtime reentry flags, or a copied validator. The existing
linear-scope capability dot does not excuse an unsafe runtime boundary.

Checkpoint: show clean focused WSTORE and GPT-2 bind baselines; reproduce the
current missing held-resident read through the real committed-model path; prove
the exact package owner on a representative definition. Stop if the private
guard requires a public interface, a caller migration outside this leaf, or a
new ownership authority.

Acceptance: the checker rejects a body that recursively opens the same
resident, while nesting different residents succeeds. A nested body failure
disposes each owner exactly once and leaves the live-owner count unchanged.
Synthetic mapped and allocated fixtures reject corrupt table offsets. Real
mapped and allocated GPT-2 tensors match their complete source spans by
SHA-256, not a prefix. Double use and use after dispose reject statically.
WSTORE and GPT-2 bind suites, the exact changed-file load paths, package and
typed-local diff gates, trust/refine gates if touched, and the combined Maki
gate pass.
