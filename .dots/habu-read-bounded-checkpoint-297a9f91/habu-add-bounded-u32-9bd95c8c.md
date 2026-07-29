---
title: Add bounded U32 store read
status: active
priority: 1
issue-type: task
created-at: "2026-07-28T17:50:23.641645+02:00"
blocks:
  - habu-return-typed-idx-6811f99f
  - habu-build-weight-table-46c9e181
---

Why: model forward needs one value-level read from an immutable weight slot; the current callback API can leak its scoped pointer.

Result: after TABLE-FROM-PAIRS replaces the public builder, package WSTORE adds U32-LE@? ( WSTORE:store CAD-NUM:index CAD-NUM:byte-off -- WSTORE:store option<n> ). The index is a slot ordinal and the offset is relative to that slot. Validate the index, offset arithmetic, and the complete four-byte window before reading. The mapped arm delegates the resulting absolute offset to SAFET:U32-LE@?; the allocated arm reads exactly four little-endian bytes. Delete WITH-SLOT, PARK, RUN-PARKED, their frame cells, tests, TRUSTED rows, and stale prose. No builder or SLOT! migration remains in this leaf because the table cut already deletes that surface.

Owner: maki/infer/weight-store.f value reads and callback removal only. Production red: no model-owned caller can read one scalar without exposing a span. Forbidden: callback or quotation API, raw pointer/span output, raw arithmetic, eager read before complete bounds proof, new trust, compatibility word, or unrelated WSTORE cleanup. Acceptance: fixed aligned and unaligned values match on mapped and allocated stores; bad slot, overflow, and every slot/arm crossing return none without touching revoked backing pages; raw index/offset role swaps reject; WITH-SLOT and its trusted helpers no longer resolve; focused WSTORE, SAFET, trust, typed-local, and package checks pass. Smallest owning check: bin/hb --load maki/infer/weight-store-test.f. Claim: agent=codex-wstore-u32 workspace=.jj-ws/habu-add-bounded-u32-9bd95c8c.
