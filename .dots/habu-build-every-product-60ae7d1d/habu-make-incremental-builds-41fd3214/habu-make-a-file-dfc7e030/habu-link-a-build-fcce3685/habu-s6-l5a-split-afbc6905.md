---
title: "S6 L5a: split the kernel emission into OPEN and KERNEL"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-05T21:33:42.758078+02:00"
closed-at: "2026-11-05T22:18:08.200548+02:00"
close-reason: "landed as \"Split the kernel emission at its open\": engine product and map byte-identical to 6f3717e8's engine (unsigned, same host), arm64-place PASS 2305 ms; three reviews, comment folds"
---

Problem: a product's boot must sit at code offset 0 before the arm64 kernel and reach its labels, but public EMIT-FORTH-IMAGE (src/habu/habu2.f:14061-14069) resets the code buffer, allocates the kernel labels and emits the kernel in one word. Design: .jj-ws.noindex/carl-dots/.out/carl-file/design-s6-l5.md L5-1. Acceptance: ENGINE-EMIT:OPEN ( ptr u8 n -- ) and ENGINE-EMIT:KERNEL ( -- ) replace EMIT-FORTH-IMAGE; FORTH = OPEN KERNEL; FORTH-C2 = checks, OPEN, offsets, KERNEL; the engine product is byte-identical (unsigned) and its map equal. Names provisional: habu-s6-l5-provisional-5f8fd488. Files: src/habu/habu2.f. Verify: brief .jj-ws.noindex/carl-dots/.out/carl-file/brief-s6-l5a.md (native-build product cmp against master 16ea4817's g2, arm64-place). Depends: none. Ownership: carl. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-s6-l5a
