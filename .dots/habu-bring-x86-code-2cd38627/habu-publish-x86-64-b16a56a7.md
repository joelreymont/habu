---
title: Publish x86-64 code without CPUID and a byte-at-a-time copy
status: closed
priority: 2
issue-type: task
created-at: "2026-11-26T14:23:36.977363+01:00"
closed-at: "2026-11-26T19:11:04.451769+01:00"
---

Problem: x86-64 code publication (src/habu/kernel-x64.f :2754 PUBLISH,) copies byte by byte where arm64 copies four bytes at a time, and ends in CODE-SYNC, (:1492, also emitted at :1504, :2737, :4259, :4420, :5384), a CPUID that exits the VM (420 ns on pulsar); a load of all src/lib spends 18.8 ms there against arm64's 2.4 ms. Intel's rule for self- and cross-modifying code puts any serializing instruction on the executing core, never the writer. Acceptance: each CODE-SYNC, site does what the x86-64 memory model requires for the code it writes and no more; publication copies a cell at a time; the engine fixpoint and the x86-64 suite pass. Depends: none. Claim: unassigned.
