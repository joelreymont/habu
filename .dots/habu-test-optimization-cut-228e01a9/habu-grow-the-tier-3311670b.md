---
title: Grow the tier-1 string literal store
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T13:16:17.969807+02:00"
---

After the trap table went (df7ecb5a, bf4c11eb), the remaining tier-1 bound on names and literals is src/compiler/native/string.f's literal store: 512 KB and 8192 bodies per pool (:18-19, :167-170), shared by every s" literal and every trap message. Measured on bf4c11eb's g1: 80 callers of no-return callees with 6995-byte names run at tier 0 but fail at tier 1 with -8636 at NSK74; 75 distinct 7000-byte s" literals fail the same way at SL74. Growing the store means opening a pool during an evaluation, which the comment above WINDOW-OPEN (string.f:205-208) forbids. Acceptance: tier 1 compiles every program tier 0 runs regardless of the count and size of its literals and trap messages, or refuses past a documented limit by name with the same verdict at tier 0; decide and state the pool rule; seen failing first through test/compiler/native-string.f.
