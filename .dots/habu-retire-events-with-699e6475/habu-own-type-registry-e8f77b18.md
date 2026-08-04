---
title: Own type registry rollback phases
status: open
priority: 1
issue-type: task
created-at: "2026-07-23T07:47:11.550005+02:00"
blocks:
  - habu-consume-registry-events-efe7fe5e
---

Problem: type-family.f installs raw combined rollback callbacks, schema owns a
separate fallible save, and restore can partly mutate before a field-depth
mismatch rejects. Counter restore also leaves rejected bytes in retired family,
string, parameter-kind, variant, field, layout, schema-node, and schema-root
tails; arena growth changes base/capacity state; snapshot persistence can bake
that history.

Owner: new sealed package `TYPE-REGISTRY-ROLLBACK` over the type-family and
type-schema registry state. Expose `RESERVE`, `SAVE`, `RESTORE-READY`,
`RESTORE`, `FINALIZE-READY`, and `FINALIZE`. `RESERVE` grows both rollback-frame
stores before either depth changes. `SAVE` is then infallible and records every
logical mark plus the base/capacity state needed for exact restoration.
Expose read-only `PROVISIONAL? ( family -- bool )`, true only while a frame is
live and the family lies in `[saved TFAM-N, current TFAM-N)` for the top frame.
`RESTORE-READY` and `FINALIZE-READY` validate both frame depths,
field-transaction depth, saved/live ranges, and growth provenance without
mutation. `RESTORE` canonicalizes every retired tail and restores the saved
logical and arena state only after readiness succeeds. Any mapping superseded
during the transaction is released exactly once without losing the primary
error. `FINALIZE` releases both frames only after readiness succeeds. Snapshot
persistence derives canonical capacity and zeroed dead bytes from committed
state, never allocation history.

Preserve serial tokens and the current restoration order: schema before
type-family. Remove raw hook installation from `type-family.f`. Do not expose
registry cells, add test setters, add a second rollback authority, or add a
trusted boundary.

Acceptance: injected growth failure in either frame or data arena leaves both
owners byte-identical, including tails, base, and capacity; readiness failure
leaves both frames live and unchanged; nested save/restore/finalize stays
lockstep; `PROVISIONAL?` is false without a frame, below the saved mark, and at
or above the live high-water, and true for every family added by the top frame;
rejected declarations produce the same snapshot and fixpoint bytes as the
untouched baseline. Mutations preserving a dirty tail, historical capacity,
leaked replacement mapping, or reversed restore order fail.

Files: `src/core/type-family.f`, `src/core/type-schema.f`, and focused rollback
suites. If a surviving source `TRUST` changes, retain only its source-local
rationale, retirement owner, and focused production test. Smallest check: a
real checker scope forces each of the eight arenas to grow, rejects after
publication, compares the complete owner state and snapshot bytes, then accepts
the same declaration; run typed-local and package gates.
