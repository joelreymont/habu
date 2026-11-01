---
title: Answer the callback thunk on x86
status: closed
priority: 2
issue-type: task
created-at: "2026-10-28T20:31:52.044889+02:00"
closed-at: "2026-11-01T15:30:22.420088+02:00"
close-reason: "Landed 3c6635d2a6 'Answer the callback thunk on x86'. On joel@pulsar-ua-106: the qsort image (sort, then -1 answering slot 0's jump target) exits 0 on the lane and 106 'slot out of range' on master a361caa5's kernel; the range, below, unbound and no-frame images exit 106 with their lines on both. Review (Opus): the kernel claims hold; its two fold-ins (comment cut to two lines, the thunk case moved into the qsort image) are in the commit. The ffi-callback rows were not run on x86: they need T0's x86 stub (C2-C4 of habu-install-t0-as-6492133d)."
---

L4-C1. Problem: x86 kernel CALLBACK-ENTRY-BODY (src/habu/kernel-x64.f:4742-4762) compares the slot unsigned against CB-POOL, so `callback-entry -1` (prims.f:1100 'answers the shared raw C thunk for a compiler-owned stub') dies 'slot out of range'; ffi-callback.f EMIT:378 calls it on every target. Acceptance: -1 answers CB-THUNK's address; slots 0..CB-POOL-1 unchanged; other negatives still refused. Files: src/habu/kernel-x64.f, an x86 kernel callback test row. Verify: on joel@pulsar-ua-106, x86-64-kernel-callback suite and ffi-callback baseline (expected still failing until L4-C3: tier 1 never fills the entry). Depends: none (kernel only). Ownership: kernel-x64.f callback rows. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-x86cb (rebased mokrpkkl onto master a361caa5).
