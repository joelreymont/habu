---
title: "Infer: serving front end"
status: open
priority: 3
issue-type: task
created-at: "2026-07-21T15:59:04.583507+02:00"
blocks:
  - habu-infer-serve-one-c0c151d2
---

This is the serving-front-end campaign record. Do not dispatch it as implementation work. Its leaves own the transport-independent request and event contracts, JSON-line transport, client backpressure, bounded HTTP framing, OpenAI-compatible mapping, connection and concurrency lifetimes, and the one-command launch. Engine and model state remain outside the transport modules.
