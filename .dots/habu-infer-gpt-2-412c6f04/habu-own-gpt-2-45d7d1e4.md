---
title: Own byte-BPE state
status: active
priority: 2
issue-type: task
created-at: "2026-07-29T20:38:10.057008+02:00"
---

Problem: the landed byte-pair encoding implementation stores tables and work buffers in singleton globals. Result: package BPE defines the linear state layout, checked capacities, OPEN, and RELEASE for immutable vocabulary and merge tables plus bounded encode/decode workspace. This leaf moves and deletes only the singleton storage and lifetime code; separate leaves own table construction, encoding, and decoding. Owner: reusable byte-BPE state lifetime only. Production red: two states cannot coexist. Acceptance: two empty states with different capacities coexist and release independently; exact, one-short, overflow, allocation, and release failures preserve ownership; no singleton BPE table or workspace remains. Forbidden: table insertion, encode/decode algorithm, asset I/O, model adapter, global storage, callback, fallback vocabulary, version, or compatibility alias. Smallest owning check: bin/hb --load maki/infer/bpe-state-test.f.

Claim: agent=codex workspace=.jj-ws/habu-own-gpt-2-45d7d1e4

Frozen interface: `OPEN` takes, in order, vocabulary `CAD-NUM:item-count`, merge `CAD-NUM:item-count`, token-arena `CAD-NUM:byte-len`, encode-work `CAD-NUM:item-count`, encode-output `CAD-NUM:item-count`, and decode-output `CAD-NUM:byte-len`, and returns generic `result<BPE:state,n>`. `RELEASE` consumes `BPE:state` and returns generic `result<n,n>`. Callers pass named capacity constants, never bare literals. The focused test distinguishes swapped adjacent vocabulary and merge capacities. Storage projections remain private inside sealed package `BPE`; no limits wrapper or special result type exists.
