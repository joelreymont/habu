---
title: Build byte-BPE tables
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T00:55:48.806989+02:00"
blocks:
  - habu-own-gpt-2-45d7d1e4
---

Why: byte-BPE vocabulary and merge construction must be atomic and independent from encoding. Interface: package BPE exposes linear BEGIN, ADD-VOCAB, ADD-MERGE, and SEAL over one BPE state and caller-authenticated entries; every refusal returns the builder unchanged and SEAL rejects missing, duplicate, or inconsistent rows before publishing the immutable state. Owner: BPE table construction only. Production red: landed tables are singleton globals. Acceptance: two builders interleave; exact merge order, duplicates, short capacity, malformed byte maps, and seal fault injection preserve owners. Forbidden: tokenizer assets, encode/decode algorithm, global table, callback, fallback vocabulary, version, or compatibility alias. Smallest owning check: bin/hb --load maki/infer/bpe-table-test.f.
