---
title: Prove ENUM kind warm snapshot
status: open
priority: 1
issue-type: task
created-at: "2026-07-24T03:11:51.011542+02:00"
blocks:
  - habu-enforce-enum-finalization-6ae2cd7b
---

Why: the finalizer must rely on existing type-family snapshot persistence, not
a second publication registry or restore hook.

Dependency: `habu-enforce-enum-finalization-6ae2cd7b` completes the cold
production path. This leaf adds no production code.

Owner and production seam: add package `ENUM-KIND-SNAPSHOT` in
`test/enum-kind-snapshot.f`. It owns one private build root, sets
`test/enum-kind-snapshot-source.f` as the sole `BUILD-EXT` keep source, invokes
the existing `BF-BUILD-STDIN-FRESH` then `BF-BUILD-SNAP-FROM-STDIN` path,
clears `BUILD-EXT` on success and failure without replacing the primary throw,
and runs the emitted `hb-new` through its real warm startup with
`--load test/enum-kind-snapshot-check.f`.

The kept source declares one full payloadless ENUM named `snapkind` through
`ENUM-DECL:ED-RUN`, so finalization chooses `TK-ENUM` before
`SNAP-RETIRE-GO` writes the image. The warm checker resolves `snapkind`
through production signatures, proves its kind, opens a real
`GENERATED-DECL` scope, and proves an attempt to finalize that older family
rejects `E-DEV-FAMILY-SCOPE` 7173 without changing kind or event publication.
It then declares and publishes a new payload ENUM as `TK-SUM`.

Checkpoint: on the exact enforcement parent, normal payloadless and payload
ENUM cases pass, the existing unmodified snapshot path builds, and the
representative new test packages pass ownership gates. Stop if any production
snapshot, build, type-family, or finalizer interface must change.

Acceptance: cold source observes `snapkind` as `TK-ENUM`; real `hb-new` exits
zero and reports the restored kind, exact 7173 rejection with unchanged family
and event state, and successful publication of the new `TK-SUM` family.
Changing the source to payload ENUM, skipping `SNAP-RETIRE-GO`, removing the
savepoint guard, mutating the restored kind, using the cold engine instead of
`hb-new`, or failing to clear `BUILD-EXT` makes an owning-path regression
fail. Two independent private-root builds produce byte-identical warm images
and both checks pass.

Exact files: `test/enum-kind-snapshot.f`,
`test/enum-kind-snapshot-source.f`, `test/enum-kind-snapshot-check.f`,
`test/gate-stdlib-cases.f` and `test/run-files.f`.

Forbidden: raw kind store, forged image, copied snapshot writer, build-tool API
change, persisted latch, registry, restore hook, or production source change.

Smallest owning check: `bin/hb --load test/enum-kind-snapshot.f`.
Claim: unassigned.
