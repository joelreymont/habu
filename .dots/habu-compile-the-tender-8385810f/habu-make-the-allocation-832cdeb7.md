---
title: Make the allocation verifier linear in ops
status: active
priority: 2
issue-type: task
created-at: "2026-09-11T16:38:06.267216+03:00"
---

Problem: A64RAV:ACCEPT 31.4 s per load, ops^1.48 in the tail; it rechecks the whole function per group. Acceptance: ACCEPT fits a slope at most 1.1 against ops with every existing rejection preserved (no validator bypass, every negative test kept, new negatives for any restructured check); controlled pair. Files: src/compiler/native/verify.f (or the A64RAV package file). Verify: allocator and verifier suites; tender-perdef scaling fit. Depends: none. Ownership: unassigned (rowan offered). Claim: unassigned


Current ownership and handoff: Owner: Rowan, .jj-ws/rowan-verify on 80716458, src/compiler/native/regalloc-verify.f only. Preserve the reviewed frame-order repair. Attribution and scaling first; retain every E-A64RAV rejection. No spill.f or regalloc.f edits.
