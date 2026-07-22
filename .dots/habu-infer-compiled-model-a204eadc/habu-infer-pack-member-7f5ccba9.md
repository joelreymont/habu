---
title: "Infer pack: member table and checksums"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:40:20.805035+02:00"
blocks:
  - habu-infer-pack-manifest-27c1030c
  - habu-pkg-sha-256-dd7221ff
---

Why this exists:
a pack needs a bounded binary member table with offsets, lengths, alignment, and content identity before any member is mapped.

Required result:
define the on-disk header and member table, checked offset arithmetic, versioning, and whole-member SHA-256 verification.

Done when:
truncation, overlap, misalignment, overflow, duplicate name, checksum mismatch, and version skew reject before any member publication; valid fixture maps deterministic spans.

Expected touch points: new maki/infer/model-pack-format.f, new maki/infer/model-pack-format-test.f, FILEMAP.md.
Smallest check: bin/hb --load maki/infer/model-pack-format-test.f.
Prerequisites: manifest schema and SHA-256 context foundation.
Owned result: binary envelope, table, and checksum validation only.
Claim: unassigned.
