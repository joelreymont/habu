---
title: Delete snap-rebase
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T18:16:44.177914+02:00"
---

Problem: the snap-rebase primitive (src/habu/habu2.f BSNAPREBASE :2576, registered :3414, its routines EMIT-CALLS :2050 and EMIT-ADDRS :2498 emitted at :3453-3455; prims.f :678; refused on x86-64 at kernel-x64.f:4432) canonicalizes a region copy for capture, which no build runs; only tests call it (test/protection-span.f TEST-SNAP-REBASE :137-139, test/x86-64-kernel-engine.f:738), and nothing in habu, zen, tender, maki or etch. Its exits (rc 95, 97) print off the line shape. Acceptance: the primitive, its row, its routines and labels, the x86-64 refusal and the tests that only call it are gone; hb builds to its fixpoint. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/snap-rebase.
