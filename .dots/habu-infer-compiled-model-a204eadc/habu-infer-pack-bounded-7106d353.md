---
title: "Infer pack: bounded tensor writer"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:40:20.816171+02:00"
blocks:
  - habu-infer-pack-member-7f5ccba9
  - habu-infer-pack-tensor-93c2e949
---

Why this exists:
pack creation must transpose, swizzle, align, or quantize tensors without materializing both complete models.

Required result:
stream one bounded chunk from the source mapping through the selected layout transform directly into the final pack member, with atomic final publication.

Done when:
configured scratch bound is never exceeded; injected read, transform, write, and checksum failures leave no published pack; source mapping is released after successful conversion.

Expected touch points: new maki/infer/model-pack-write.f, new maki/infer/model-pack-write-test.f, FILEMAP.md.
Smallest check: bin/hb --load maki/infer/model-pack-write-test.f.
Prerequisites: member table and checksums, tensor layout catalog.
Owned result: bounded writer and publication transaction only.
Claim: unassigned.
