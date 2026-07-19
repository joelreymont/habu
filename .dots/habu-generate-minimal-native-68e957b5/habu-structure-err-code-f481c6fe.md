---
title: Structure error code ownership
status: open
priority: 1
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T21:36:34.305186+02:00"
---

Evidence: tools/error-code-lint-core.f:42-50,140-213 stores claims in three parallel arrays and reservations in four. Name-id, stem-id, and file-id all share n, so wrong-column writes silently reassign ownership. Reservation state overloads first=0 and last=0 as absence, and ECL-RES-FIND returns -1. Introduce nominal intern, file, and stem identifiers; a checked STRUCTURE claim; a payload ENUM reservation-state with none, first-only(code), last-only(code), and complete(first,last) variants; option lookup; and LAYOUT-BUFFER storage. Make find-or-create and FIRST/LAST updates exhaustive and transactional. Preserve exact lint policy and diagnostics. Prove FIRST/LAST in either order, duplicates, incomplete ranges, foreign-range detection, compile-negative field/id swaps, malformed input rollback, capacity/canaries, and exact diagnostics. Measure source definitions, JIT/DATA bytes, storage, and scan time before and after.
