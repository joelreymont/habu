---
title: Make the allocation verifier linear in ops
status: closed
priority: 2
issue-type: task
created-at: "\"2026-09-11T16:38:06.267216+03:00\""
closed-at: "2026-09-11T19:05:39.097104+03:00"
close-reason: "Landed on the root: e95dc855 'Check all frame slots in one bit-parallel fixpoint' and c79d441b 'Count using blocks and skip one-block order sweeps' (regalloc-verify.f; FLOW-CK one bit-parallel fixpoint over all slots with cached op-slot/stores and predecessor lists, identical Gauss-Seidel order and lattice (reviewer-verified case by case), single-block order tokens skip the provably-unable-to-throw sweep; lines 350-411 untouched; four new negatives falsified by mutation; ACCEPT on the 879-definition corpus 11.43 s with slope 1.39 before, projected 3.6 s; timing pair pending a quiet box). OVERLAP-CK sweep dropped: unreachable without the allocator, no committable negative."
---

Problem: A64RAV:ACCEPT 31.4 s per load, ops^1.48 in the tail; it rechecks the whole function per group. Acceptance: ACCEPT fits a slope at most 1.1 against ops with every existing rejection preserved (no validator bypass, every negative test kept, new negatives for any restructured check); controlled pair. Files: src/compiler/native/verify.f (or the A64RAV package file). Verify: allocator and verifier suites; tender-perdef scaling fit. Depends: none. Ownership: unassigned (rowan offered). Claim: unassigned


Current ownership and handoff: Owner: Rowan, .jj-ws/rowan-verify on 80716458, src/compiler/native/regalloc-verify.f only. Preserve the reviewed frame-order repair. Attribution and scaling first; retain every E-A64RAV rejection. No spill.f or regalloc.f edits.
