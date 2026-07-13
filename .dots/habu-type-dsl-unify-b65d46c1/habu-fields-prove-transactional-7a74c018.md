---
title: "Fields: prove transactional metadata"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:13:09.839382+02:00"
blocks:
  - habu-fields-retire-value-fb574935
---

Own shared field snapshot, rollback, hashing, and public read-only reflection integration plus focused tests. Prove declaration failure restores every watermark, baked snapshot round-trips field identity/layout, and reflection exposes names/types/owners without mutable pointers. Validate family, engine, snapshot, trust, and filemap gates.
