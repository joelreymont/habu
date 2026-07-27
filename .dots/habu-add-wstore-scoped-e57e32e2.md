---
title: Add WSTORE scoped read over held resident
status: active
priority: 2
issue-type: task
created-at: "2026-07-26T17:11:32.436417+02:00"
---

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
`habu-add-wstore-public-db6c70fe`, plus the total SAFET mapping contract
`habu-return-typed-mem-ac35e3c9`.

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

Claim: agent=wstore-scoped workspace=.jj-ws/habu-add-wstore-scoped-e57e32e2
