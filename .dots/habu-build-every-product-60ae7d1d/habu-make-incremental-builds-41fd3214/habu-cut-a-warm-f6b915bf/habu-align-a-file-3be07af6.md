---
title: Read an unaligned little-endian cell in one access
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T06:03:12.023947+02:00"
closed-at: "2026-11-13T06:22:04.294298+02:00"
---

Problem: LE:U64@ (src/lib/le.f:35) runs 34.1M times in a warm engine build and 12.7M (37%) take its byte loop, because NATIVE-SLOT? (:18) sends every unaligned address there; arm64, x86-64 and Wasm all take an unaligned cell access, and checked code's CELL-VIEW @ at an odd address answers right (258 stored and fetched at XQ 3 +). File objects lay cells at any offset (src/habu/linker.f WRITE-OBJECT :2556, FIELD+ :532), so WORDS-CK, PLACE, EXTENTS-CK, RD-REF and PATH-IN read through the byte loop. Acceptance: U64@ and U64! take one access at any address; byte-loop calls and warm engine build time measured before and after; warm equals cold. Depends: none. Claim: unassigned.
