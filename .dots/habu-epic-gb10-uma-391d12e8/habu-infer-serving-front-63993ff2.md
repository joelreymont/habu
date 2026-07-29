---
title: "Infer: serving front end"
status: open
priority: 3
issue-type: task
created-at: "2026-07-21T15:59:04.583507+02:00"
blocks:
  - habu-infer-serve-one-c0c151d2
---

Campaign only; do not dispatch. After real GPT-2 batching, add only the missing socket libc boundaries; implement bounded POST `/v1/completions` HTTP/1.1 framing, the exact non-stream OpenAI completion subset, one connection owner, one single-thread poll server, and one direct serve command. Request decode uses caller storage. Response render uses the existing JSON-WRITE only synchronously with no yield or nesting and immediately copies the complete body into connection storage. SCHED owns request state, result-row schema, and the only writer; each TICK caller owns its table and arena; SERVE owns that storage in the product plus listener and connections. HTTP-COMP owns framing; OPENAI-COMP owns JSON. No layer duplicates another.

The endpoint supports one prompt string, one choice, one Content-Length JSON response, strict immediate admission, Connection: close, and explicit model and capacity arguments. There is no streaming, JSON-line protocol, generic HTTP framework, keep-alive, chat endpoint, task, thread, mutex, pack, plugin, reload, auto-detection, schema, generated client, metrics, or compatibility route. Close when fresh-process `--once` serves both real model arms and releases every owner.
