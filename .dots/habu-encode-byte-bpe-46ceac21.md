---
title: Encode byte-BPE piece
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T00:55:48.928128+02:00"
---

Why: piece encoding needs one bounded merge transaction over an immutable BPE owner. Interface: BPE:ENCODE takes and returns a sealed BPE state plus caller input/output spans, uses state-owned bounded merge workspace, applies exact rank order, and publishes output only after capacity preflight. Owner: byte-BPE piece encoding only. Production red: encoding uses singleton work buffers. Acceptance: landed fixtures, rank ties, empty or malformed bytes, short output, and two-state interleaving pass with no partial output. Forbidden: model splitting, asset I/O, table construction, decode, allocation, global scratch, callback, version, or compatibility alias. Smallest owning check: bin/hb --load maki/infer/bpe-encode-test.f.
