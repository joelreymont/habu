---
title: "Infer: serving front end"
status: open
priority: 3
issue-type: task
created-at: "2026-07-21T15:59:04.583507+02:00"
blocks:
  - habu-infer-continuous-batching-a55e2cb5
---

Phase 5: the serving surface. Stage 1: a stdin/stdout line protocol on the engine (prompt in, streamed tokens out, JSON framing per the existing tools/json.f machinery) - enough for local use and scripted evaluation. Stage 2 (own decision point when reached): HTTP with an OpenAI-compatible completions endpoint over the process/fd machinery; streaming via chunked responses. Keep the server strictly a front end - all engine logic stays in the engine module; the server owns no model state. Red-first protocol tests host-only (malformed requests reject named, backpressure honest, concurrent clients isolated through the batching scheduler).
