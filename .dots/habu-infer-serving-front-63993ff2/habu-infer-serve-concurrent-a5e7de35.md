---
title: "Infer serve: concurrent HTTP server"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:44.232595+02:00"
blocks:
  - habu-infer-serve-http-4fb09e9a
---

Why this exists:
The product requires multiple isolated clients feeding the continuous scheduler through one loaded model.

Required result:
Accept bounded concurrent connections, assign one connection owner per client, and route validated requests and events through the shared scheduler without duplicating model state.

Done when:
Mixed clients stream correct isolated responses; admission waits remain honest; one slow, malformed, or cancelled client cannot corrupt another; shutdown drains or cancels every owner.

Expected touch points: HTTP accept loop, concurrency integration, and focused multi-client tests.
Smallest check: the focused concurrent-client integration test.
Prerequisites: HTTP connection lifecycle and scheduler churn proof.
Owned result: bounded multi-client HTTP coordination only.
Claim: unassigned.
