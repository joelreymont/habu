---
title: Answer the callback thunk on x86
status: active
priority: 2
issue-type: task
created-at: "2026-10-28T20:31:52.044889+02:00"
---

L4-C1. Problem: x86 kernel CALLBACK-ENTRY-BODY (src/habu/kernel-x64.f:4742-4762) compares the slot unsigned against CB-POOL, so `callback-entry -1` (prims.f:1100 'answers the shared raw C thunk for a compiler-owned stub') dies 'slot out of range'; ffi-callback.f EMIT:378 calls it on every target. Acceptance: -1 answers CB-THUNK's address; slots 0..CB-POOL-1 unchanged; other negatives still refused. Files: src/habu/kernel-x64.f, an x86 kernel callback test row. Verify: on joel@pulsar-ua-106, x86-64-kernel-callback suite and ffi-callback baseline (expected still failing until L4-C3: tier 1 never fills the entry). Depends: none (kernel only). Ownership: kernel-x64.f callback rows. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-l4k.
