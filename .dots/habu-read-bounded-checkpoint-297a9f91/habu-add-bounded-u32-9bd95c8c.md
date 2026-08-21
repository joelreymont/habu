---
title: Add bounded U32 store read
status: closed
priority: 1
issue-type: task
created-at: "2026-07-28T17:50:23.641645+02:00"
closed-at: "2026-08-02T16:00:35.780081+02:00"
close-reason: Superseded by commit 5b0ebb, which deleted the WSTORE/GPT2LOAD path; retaining this task would resurrect the removed design.
---

Claim: RELEASED 2026-07-29 by the stale-claim audit. Agent `codex-wstore-u32` and workspace `.jj-ws/habu-add-bounded-u32-9bd95c8c` are both gone: the directory does not exist and `jj workspace list` has no record of it. The work has not landed - `rg 'U32-LE@' maki/infer/weight-store.f` returns nothing and the `RUN-PARKED`/`PARK` trust rows are still in `TRUSTED.md`. The dot stays active and is free to claim.

Result: package WSTORE adds U32-LE@? ( WSTORE:store CAD-NUM:index CAD-NUM:byte-off -- WSTORE:store option<n> ). The index is a slot ordinal and the offset is relative to that slot. Before either arm reads, validate the slot, the slot-relative four-byte window, overflow in the absolute offset, and the complete arm-relative window. The mapped arm delegates the validated absolute offset to SAFET:U32-LE@?; the allocated arm reads exactly four little-endian bytes. Every invalid value returns none while preserving the store. Hard-cut SLOT!, its row helpers, and every live caller from raw n to CAD-NUM:index in the same commit; keep TABLE-NEW, SLOT!, SEAL, and the existing table layout. Delete WITH-SLOT, PARK, RUN-PARKED, their frame cells, tests, TRUSTED rows, and stale prose.

Owner: package WSTORE in maki/infer/weight-store.f owns the value read, slot-index hard cut, and callback removal; existing GPT2LOAD callers migrate only to the typed SLOT! input. Production red: WSTORE:U32-LE@? is undefined, so no loaded model can read one scalar without exposing a span. Forbidden: TABLE-FROM-PAIRS or any builder redesign, callback or quotation API, raw pointer/span output, raw index or offset at a public boundary, eager read before the complete proof, throw for a read refusal, new trust, compatibility word, or unrelated WSTORE cleanup. Acceptance: the pre-change production call is rejected as undefined; fixed aligned and unaligned values match on mapped and allocated stores; last-valid and first-invalid four-byte windows, bad slot, arithmetic overflow, and every slot/arm crossing return the exact option result without touching revoked backing pages; all SLOT! callers use CAD-NUM:index; raw index/offset role swaps reject; WITH-SLOT and its trusted helpers no longer resolve; focused WSTORE, SAFET, GPT2LOAD, trust, typed-local, package, and exact-diff checks pass. Smallest owning check: bin/hb --load maki/infer/weight-store-test.f. Claim: agent=codex-wstore-u32 workspace=.jj-ws/habu-add-bounded-u32-9bd95c8c.
