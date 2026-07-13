---
title: Publish canonical checked BYTE-VIEW
status: active
priority: 1
issue-type: task
created-at: "2026-07-13T16:05:05.316227+02:00"
---

Full context: pointer arithmetic, strict pointee invariance, and the checker ptr a to ptr u8 rule are already landed, but callers still lack one canonical checked public introduction word. Fix: add BYTE-VIEW ( ptr a -- ptr u8 ) in the core byte vocabulary, document and manifest it, and pin positive access plus BYTE-VIEW @ and BYTE-VIEW ! nominal/pointee negatives. This leaf does not own startup-image, builder, structures, or bounded-region migrations. Acceptance: public BYTE-VIEW certifies, preserves address identity, permits checked byte access only, cannot widen back to arbitrary cells or nominal values, and adds no trust row. Files: src/core/bytes.f, lib/string-test.f, lib/std.manifest, docs/stdlib.md. Verify: string test, engine pointer negatives, typed-local diff lint, trusted inventory, host/filemap/status lints, full native gate.

Claim: agent=byte-view workspace=.jj-ws/sol-byte-view.
