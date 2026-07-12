---
title: "V2 R3: preserve kinds in persistence"
status: open
priority: 1
issue-type: task
created-at: "2026-07-12T07:09:45.774985+02:00"
blocks:
  - habu-v2-r3-type-dfe5609e
  - habu-v2-r3-type-9f89d1e9
  - habu-v2-r3-type-5809bec6
  - habu-v2-r3-type-5a20bd12
  - habu-v2-r3-type-2f60c17c
---

Problem: string keys and raw table cells can erase nominal CAD family identity after in-memory APIs are migrated. Fix: make canonical record/codec boundaries encode an explicit kind/schema discriminator, validate it before private refinement, and keep typed records internally; cache keys derive from canonical typed fields rather than ad hoc n/string concatenation. Acceptance: encode/decode round-trips every R3 kind; decoding one kind as another rejects; kind changes alter canonical hash/key; rollback and snapshot replay retain family identity. Files: maki/store.f, canonical codec/artifact-store owners, focused replay tests. Verify: byte-stable codec fixtures, store replay tests, maki/test.f. Depends: all R3 owner migrations.
