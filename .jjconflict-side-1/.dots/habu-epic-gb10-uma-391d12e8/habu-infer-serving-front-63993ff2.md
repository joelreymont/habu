---
title: "Infer: serving front end"
status: open
priority: 3
issue-type: task
created-at: "2026-07-21T15:59:04.583507+02:00"
blocks:
  - habu-infer-serve-one-c0c151d2
---

Campaign only; do not dispatch. After real GPT-2 batching, add only the missing socket libc boundaries; implement bounded POST `/v1/completions` HTTP/1.1 framing, the exact non-stream OpenAI completion subset, one connection owner, one single-thread poll server, and one direct serve command. Argument parsing records only requested syntax. The command opens the selected model, starts the engine, obtains canonical INFER:info, starts SCHED, then SERVE alone plans and owns product storage. Each connection owns one explicit JSON-WRITE:writer, scratch, stable response, and HTTP wire span; OPENAI-COMP renders into that writer and SERVE-CONN publishes through JSON-WRITE:COPY. SCHED owns request state, result-row schema, capacity calculation, and the only result writer; SERVE owns the one product table and arena plus listener and connections. HTTP-COMP owns framing; OPENAI-COMP owns JSON. No layer duplicates another.

The endpoint supports one prompt string, one choice, one Content-Length JSON response, strict immediate admission, Connection: close, and explicit model and capacity arguments. There is no streaming, JSON-line protocol, generic HTTP framework, keep-alive, chat endpoint, task, thread, mutex, pack, plugin, reload, auto-detection, schema, generated client, metrics, or compatibility route. Close when fresh-process `--once` serves both real model arms and releases every owner.
