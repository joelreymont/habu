---
title: Pin wide ADT protected-store guard
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T12:34:17.023135+02:00"
---

Problem: commit 69fb5059 emits a two-band seal guard for every cell of wide !, but current runtime regressions only write ordinary CREATE buffers; an offset or loop change could let a later cell cross into protected memory. Fix: add a focused generated checked fixture that constructs a W=2 or W=4 ADT, presents a ptr family address whose first or later destination cell intersects a sealed protected band, executes wide !, and proves named E-SEAL-VIOLATION with no partial protected mutation. Use existing seal fixture/debug infrastructure; do not add TRUSTED unless the address-construction boundary already exists and is documented. Acceptance: ordinary wide store remains green; first-cell and later-cell protected intersections fail closed at the sink; protected bytes remain unchanged; test runs through native bin/hb candidate and is registered in the owning engine gate. Files: new focused test fixture plus the smallest owning test/gate-engine-lib.f registration; no checker/compiler edits. Verify: bootstrap-produced candidate focused fixture, engine gate, typed-local-diff-lint, filemap-lint. Depends: none; implementation commit 69fb5059 is already on the integration line.
