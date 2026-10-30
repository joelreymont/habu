---
title: Make the engine fixpoint independent of its seed
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T02:17:43.015200+02:00"
---

Problem: on phil's comma lane tip (cb2b120a, master a94c27e7) the native self-rebuild chain (phil's g3 -> g1 -> g2, g1 == g2) and the recovery chain (tools/bootstrap.sh -> hb-stdin -> rec -> rec2 -> rec3, rec2 == rec3) converge to engines that differ by 40 bytes at file offset 2676685 plus the 32-byte trailing digest: a table of 8-byte (offset, value) entries in which $2D90 sits in sorted order in the recovery fixpoint and after $37E8 in the native one (comma lane 2 review; the reviewer's images are gone, the chains must be rebuilt). Not known: which table, whether its writer orders entries by the host's state, and whether master without the lane shows it. Acceptance: the table and its writer named; if the order depends on the seed, the writer produces a canonical order and both chains rebuilt from this tree compare byte-identical (hb and hb.map); if the order is canonical and the difference has another cause, that cause is named with evidence. Files: the table's writer (tools/native-build*.f, src/habu/snap-lib.f or the image layout). Verify: cmp of the two chains' fixpoints; two-generation convergence. Depends: none. Ownership: the image writer. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-fixpoint (a duplicate owtmlsmu of the lane tip).
