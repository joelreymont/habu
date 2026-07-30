---
title: Decode byte-BPE identifiers
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T00:55:49.041968+02:00"
blocks:
  - habu-build-byte-bpe-b915f751
---

Why: identifier decoding needs one bounded expansion transaction independent from encoding. Interface: BPE:DECODE takes and returns a sealed BPE state plus caller identifier/output spans, validates every identifier and complete byte bound, and writes only after preflight. Owner: byte-BPE identifier decoding only. Production red: decoding uses singleton tables and workspace. Acceptance: landed byte mapping, multi-id expansion, invalid id, overflow, short output, and two-state interleaving pass with unchanged output on refusal. Forbidden: model stop policy, asset I/O, table construction, encode, allocation, global scratch, callback, version, or compatibility alias. Smallest owning check: bin/hb --load maki/infer/bpe-decode-test.f.
