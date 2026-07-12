---
title: "Seal owner packages: persisted registry"
status: open
priority: 2
issue-type: task
created-at: "2026-07-12T15:41:55.088859+02:00"
---

Implement phase 1 of habu-checker-seal-owner-f7de26ff. Reserve protected owner-package pair storage in the sealed DATA band with count plus atomic {public-wid,private-wid} rows; native membership predicates distinguish callable public from hidden private; cold init, snapshot, AOT capture/bake/restore, WIDN advancement, capacity validation, and bootstrap layout parity. Registry remains unpopulated in this phase. Never reuse constructor protected-WID semantics because EM-AOTWIDGATE rejects those WIDs. Acceptance: zero-population native fixpoint byte/layout tests, snapshot/AOT round-trip fixtures for injected rows, exact capacity/one-short preflight without partial stores, old images fail closed rather than reinterpret offsets. Files: src/habu/layout.f, habu1.f, habu2.f, aot-capture.f, bootstrap/cg/forth.fs, layout/AOT tests. Full native/bootstrap/AOT/test gates.
