---
title: Inline the views and constants T0 calls
status: active
priority: 1
issue-type: task
created-at: "2026-11-25T16:32:56.716437+01:00"
---

Problem: byte-view, cell-view, NULL-PTR and data-base are calls in T0 code (src/habu/t0.f OPS-MAP), though each is at most one instruction; inlining the first three made a compile 3.6% faster on x86 and 5.5% on arm64. Acceptance: those words compile to no call, and the load is faster on both. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-x-inl.
