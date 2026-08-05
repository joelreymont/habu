---
title: Vectorize the byte loops with NEON
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T09:41:37.405181+02:00"
---

The counted byte/cell scan loops (BYTE-SUM, BYTE-FIND, COUNT-CHAR shapes) are memchr-class: 8-16x is normal with 16-byte NEON strips plus a scalar tail. Scope strictly to the loop shapes the elaborator already recognizes (single induction, single array, pure body of the vectorizable ops); each NEON encoder goes through asm.f + the Rocq model per the Csel precedent (ld1, and/orr/add vector forms, addv/uminv reductions, cmeq+shrn movemask idiom); answers pinned including the alignment head/tail edges and the empty and single-byte inputs. Acceptance: measured against the clang column (clang auto-vectorizes these — this is where the biggest gaps will sit); the sweep tool gains a vector-loop leaf so placement effects on the strip loop are known.

Blocked by: habu-epic-hard-cut-a684f24d phases 1-6. Re-scoped: only recognized induction/access/reduction shapes with complete scalar head/tail handling and exact empty/short/aligned/unaligned goldens; start with byte scan/find/count workloads where clang shows a large measured gap.
