---
title: "Hear every client close in ws-test's capacity case"
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T18:26:34.088066+02:00"
---

Problem: src/lib/aio.f:380-398 reads and writes io_uring's ring indices with LE:U32@ and LE:U32!, four byte accesses each, while the kernel reads and writes the same words; an index passing a multiple of 256 can tear. A torn head makes the kernel post completions to its overflow list, which AIO never flushes; a torn tail completes stale entries and spins holding AIO-LOCK. Any server on AIO can hang; ws-test's CAPACITY-CASE did. Acceptance: each index access is one 32-bit load or store ordered as atomic@ and atomic! are, a test that reads the tail while completions arrive never sees it step back, and forced torn indices no longer occur. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-aio-idx.
