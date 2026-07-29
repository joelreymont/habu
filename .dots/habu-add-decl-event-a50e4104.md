---
title: Make declaration closure manifest authoritative
status: active
priority: 2
issue-type: task
created-at: "2026-07-21T06:59:26.054283+02:00"
---

Problem: `BF-APPEND-DECL-FILES` hardcodes four declaration sources—
`decl-event.f`, `structure-make.f`, `structure-decl.f`, and `enum-decl.f`—but
none belongs to the stdin closure manifest or the candidate content key. An
edit to any of them can therefore reuse a stale candidate. Adding only
`decl-event.f` would preserve the same defect for its three consumers.

Acceptance: make `tools/stdin-closure-lib.f` the sole ordered authority for all
four declaration build inputs. Add a declaration-input role distinct from the
existing host role; every declaration row is keyed but is not a metabuild-host
component. `BF-APPEND-DECL-FILES` walks those rows in manifest order and contains
no declaration path literal. The candidate key walks every keyed manifest row
directly instead of duplicating those paths in `test/run-files.f`. The audited
bootstrap launcher keeps its required literal paths, and
`tools/stdin-closure-lint.f` proves that it contains every declaration row,
that build-fixpoint consumes the declaration role structurally, and that no raw
declaration path bypass remains there. `tools/srclist.f` remains limited to
metabuild-host rows; declaration inputs must not be misclassified as host code.

Tests independently mutate each of the four declaration files and prove that
the candidate key changes. Removing, reordering, duplicating, or changing the
role of any declaration row makes a focused fixture fail. Preserve the exact
load order and generated engine bytes. Files: `tools/stdin-closure-lib.f`,
`tools/build-fixpoint.f`, `test/run-files.f`, `test/run-lib.f`,
`tools/stdin-closure-lint.f`, and focused lint/key fixtures. Verify the focused
fixtures, candidate cache miss/hit behavior, native fixpoint twice, and the full
native gate.

Claim: RELEASED 2026-07-29 by the stale-claim audit. Agent `decl_event_manifest` and workspace `.jj-ws/habu-add-decl-event-a50e4104` are both gone: the directory does not exist and `jj workspace list` has no record of it. The work has not landed - `tools/build-fixpoint.f` still hardcodes the four declaration source literals in `BF-APPEND-DECL-FILES`. The dot stays active and is free to claim.
