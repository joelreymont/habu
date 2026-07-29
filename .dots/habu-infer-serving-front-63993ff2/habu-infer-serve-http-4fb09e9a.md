---
title: "Infer serve: HTTP connection lifecycle"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:44.072966+02:00"
blocks:
  - habu-infer-serve-openai-1dca13cd
---

Why this exists:
A socket connection needs explicit ownership across accept, request framing, response writes, client cancellation, and shutdown.

Required result:
Drive one bounded HTTP connection from accepted descriptor through framing and OpenAI mapping to a client backpressure lease, closing every descriptor and request owner exactly once.

Done when:
Normal, keep-alive-disabled, malformed, slow-reader, disconnect, timeout, engine-error, and server-shutdown traces all release their owners and preserve other connections.

Expected touch points: HTTP connection driver and focused socket lifecycle tests.
Smallest check: the focused connection lifecycle test.
Prerequisites: HTTP framing, OpenAI mapping, and client backpressure lease.
Owned result: one HTTP connection lifecycle only.
Claim: unassigned.
