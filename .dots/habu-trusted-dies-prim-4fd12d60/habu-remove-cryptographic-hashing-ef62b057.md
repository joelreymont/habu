---
title: Remove cryptographic hashing from internal symbol lookup filter
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-11T16:07:57.869453+03:00\""
---

Owner: Rowan symbol lane .jj-ws/rowan-symbol; src/compiler/ir/symbol.f only, base41df9051. Current FILTER uses SHA256 roughly400000 times per Tender compile, sampled10.6s/9.6%. This16-bit internal intern filter never serializes and full equality confirms matches; canonical content digests elsewhere must remain unchanged.

Acceptance: replace only the unnecessary internal lookup filter cost with a suitable deterministic fast hash, preserve collision/equality and symbol-identity behavior, canonical serialization/digests unchanged. Run focused interning/collision tests and same forced-AOT Tender before/after, with real NCOMP counts. No new hash framework or global cryptographic replacement. First measurements still pending as of2026-09-11 13:04UTC.

Rowan corrected measurement13:16UTC: 2,905,869 interns per load, not400000; symbol filter causes99.55% of CDIGEST:COMPUTE calls. Timed SHA filter15.07s versus proposed FNV-1a64 xor-folded filter0.14s. This is isolated filter timing, not a completed full-workload gain. Candidate implementing now; canonical filter consumers audit complete, only private ROW-MATCH?/ROW-ADD use filter. Existing symbol collision fixture changes from VGA/HRA to jest/yank(0x615E), equality assertions retained. Source/proof comments updated without changing arbitrary-filter proof statements.
