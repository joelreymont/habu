---
title: Save images holding tier 0 code
status: active
priority: 3
issue-type: task
created-at: "2026-10-26T11:28:42.487638+02:00"
---

Retirement prerequisite (Joel 2026-10-26): do only if the experiment ruling retires tier 1. PERSIST requires code-origin 1 (src/habu/snap-lib.f:684-688); no image contains tier 0 code (src/habu/jit.f:7-8).

Joel 2026-10-27: "make it happen in your branch and test it out" - build tier 0 AOT products and images in a workspace so servers (zen, maki) can be compared AOT tier 0 against AOT tier 1. Claim: jerry. Design: Plan/Fable over master 2b7162ca (workspace .jj-ws.noindex/jerry-t0).
