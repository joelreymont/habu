---
title: Prove concurrent completion clients
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T07:03:54.676862+02:00"
blocks:
  - habu-infer-serve-concurrent-a5e7de35
  - habu-infer-batch-decode-7df51c5c
---

Why: the product server must demonstrate isolation and batching with real sockets, not a copied state machine. Result: add one production-path loopback integration using SOCK-OS:CONNECT, the real SERVE:RUN-ONCE, SCHED, and GPT-2. Three simultaneous clients fragment or delay request bytes independently; two admitted requests share one NEXT-MANY launch while the slow peer remains isolated; responses and Content-Length bodies are exact; every connection, request, page, descriptor, writer, and server owner is released. Owner: concurrent server integration proof only; no production behavior. Dependency: completed RUN-ONCE and real batched GPT-2. Production red: no owning check proves the poll, scheduler, routing, and socket implementations compose for several clients. Acceptance: no-readiness scheduler progress, slow-client isolation, shared batch launch, would-block, cleanup failure, and immediate port reuse execute through the real production entry points. Forbidden: synthetic scheduler, copied client, second server path, benchmark, metric, lint, compatibility, or production code. Smallest owning check: bin/hb --load maki/serve/server-concurrent-test.f on DGX Spark.

Claim: unassigned.
