---
title: "Seal owners: persisted registry"
status: open
priority: 1
issue-type: task
created-at: "2026-07-12T16:07:10.444030+02:00"
---

Problem: no persisted distinction exists between callable sealed-owner public WIDs and hidden private WIDs; constructor protection has incompatible AOT semantics. Acceptance: reserve atomic public/private WID-pair storage in sealed DATA; native membership predicates distinguish roles; zero-population cold init, snapshot, AOT capture/bake/restore, WIDN advancement, capacity and one-short preflight are proven without partial stores; bootstrap layout matches. Files: src/habu/layout.f, src/habu/habu1.f, src/habu/habu2.f, src/habu/aot-capture.f, bootstrap/cg/forth.fs, layout and AOT tests. Verify: native fixpoint, snapshot, AOT, bootstrap, full test/run.f. Depends: none. Ownership: only protected registry layout, persistence, and role predicates; no checker grammar or owner migrations.
