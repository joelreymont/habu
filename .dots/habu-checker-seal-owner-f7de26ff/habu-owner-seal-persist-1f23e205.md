---
title: "Owner seal: persist pair registry"
status: open
priority: 1
issue-type: task
created-at: "2026-07-12T16:18:25.951959+02:00"
blocks:
  - habu-owner-seal-reserve-dee3c76c
---

Problem: reserved owner public/private WID pairs must survive snapshot, AOT capture/bake/restore and recovery bootstrap without constructor AOT rejection semantics. Acceptance: snapshot copies/restores pairs with zero ephemeral authority; AOT captures count and pairs, validates capacity before copy, advances WIDN past both roles, permits public calls and hides private WIDs; bootstrap layout mirror matches; malformed old/new images fail closed. Files: src/habu/aot-capture.f, src/habu/habu2.f AOT restore/bake, bootstrap/cg/forth.fs, snapshot/AOT/bootstrap tests. Verify: snapshot round trip, AOT public/private proof, recovered bootstrap, fixpoint and full test/run.f. Depends: habu-owner-seal-reserve-dee3c76c. Ownership: persistence/bootstrap only; no checker registry, syntax or CAD migration.
