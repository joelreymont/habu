---
title: "TFAM 13: ADT diagnostics + repair packets + public signatures"
status: open
priority: 2
issue-type: task
created-at: "2026-07-03T23:36:48.953573+02:00"
---

PLAN.md item 13. Bounded/growable row collection; hidden-field runs render as logical family<args>; machine-readable ADT fields (family id/name, arity, variant/tag, payload pos, expected/actual) end-to-end through repair packets + gate JSON assertions; non-definition declaration-error packet shape (no fake word/effect fields); public signatures synthesized from TFAM/SUMV metadata, never hidden fields; (CMP) paren-word lexer parity; GJA-SUGGEST-FOR for every new class. Gate 17m. Depends: TFAM 5, 7, 8, 9, 12.

DESTRUCTION-REVIEW FINDINGS FROM ITEM 6 (2026-07-04, assigned here — diagnostics scope): (S2) verify-source.f:387-398 RECORD-TYPEFAMILY/RECORD-SUMTYPE pre-checks (missing name/arity/;SUMTYPE) hard-die 74 with no declaration packet, and tools/check-core.f:730,:753 pre-checks raw CHK-THROW — native path emits the TDECL-DIAG packet for the same input (docs/type-families.md section 24 requires the packet on every path); route pre-check rejects through the declaration packet + multi-error counting. (S3) tools/check-all-errors-core.f CA-ADD-SUPPORT-TRIPLE/CA-ADD-SUPPORT-SUM silently skip an EOF-truncated TYPEFAMILY/unterminated SUMTYPE (no support row, no diagnostic) — collector must report truncated declarations. (C1) TYPEFAMILY ptr 0 rejects as E-TFAM-DUP not reserved-name (PLAN.md:531 framing) — reclassify or document. (C2) oversized declaration bodies (sumtype.f TDECL-C, over $1000; check-core buffer paths) die raw without a packet. Each needs a fixture proving packet shape parity across native/verify-source/check-core paths.
