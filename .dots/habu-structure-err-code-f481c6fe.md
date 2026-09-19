---
title: Structure error code ownership
status: active
priority: 1
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T21:36:34.305186+02:00"
---

Evidence: tools/error-code-lint-core.f:42-50,140-213 stores claims in three parallel arrays and reservations in four. Name-id, stem-id, and file-id all share n, so wrong-column writes silently reassign ownership. Reservation state overloads first=0 and last=0 as absence, and ECL-RES-FIND returns -1. Introduce nominal intern, file, and stem identifiers; a checked STRUCTURE claim; a payload ENUM reservation-state with none, first-only(code), last-only(code), and complete(first,last) variants; option lookup; and LAYOUT-BUFFER storage. Make find-or-create and FIRST/LAST updates exhaustive and transactional. Preserve exact lint policy and diagnostics. Prove FIRST/LAST in either order, duplicates, incomplete ranges, foreign-range detection, compile-negative field/id swaps, malformed input rollback, capacity/canaries, and exact diagnostics. Measure source definitions, JIT/DATA bytes, storage, and scan time before and after.

Claim: alder, .jj-ws/alder-error-ownership from b4efad25. Preserve lint policy and diagnostics; typed ids/records and exhaustive reservation state own the change.

Ready for Hazel review: claims and reservations now use checked records in LAYOUT-BUFFER storage. Three private nominal ids prevent name/file/stem swaps. A reservation holds none, first-only, last-only or complete; find returns option<n>. New rows are fully constructed before publication; endpoint updates are exhaustive and preserve the opposite bound. Negative-zero clearing and the public query's zero-for-absent compatibility remain explicit at their boundaries. No parser policy change.

Validation: whole error-code-lint-fixtures and error-code-region rows pass; new fixtures also pass at tier 0. Tests cover bound order, replacement, absent-bound query compatibility, same stem/different owners, rejected id/field swaps, both full tables and outside indices, and malformed-source rollback. Public behavior fixtures pass on the old implementation too. Collision/foreign-range diagnostics and both capacity exits compare byte-for-byte against the old implementation (capacity rc 1). Live ledger: 1097 files, 722 claims, 57 reservations, zero findings. Independent Astra review and targeted follow-up clear. No full gate or engine changes.

Measurements on the same private host: colon definitions 59 -> 65, plus six private CAST declarations; dictionary delta for loading the core and dependencies 728 -> 753. JIT delta 54,472 -> 57,016 bytes; DATA delta 1,839,575 -> 1,847,767 bytes. Claim rows remain 3 cells (24 bytes); reservation rows 4 -> 5 cells (32 -> 40 bytes), total table storage 81,920 -> 90,112 bytes. Three full-tree scan median 1.463724792 -> 1.467646750 seconds (~0.27%, within timing noise). No durable benchmark script.
