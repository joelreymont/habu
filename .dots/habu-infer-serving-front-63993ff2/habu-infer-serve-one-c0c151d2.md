---
title: Serve completions from one command
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:44.376529+02:00"
blocks:
  - habu-infer-serve-concurrent-a5e7de35
  - habu-infer-dense-full-14833530
  - habu-infer-batch-decode-7df51c5c
  - habu-parse-serve-cmd-07cb5e18
---

Why: the engine is not a usable vLLM replacement until one native command loads a real model and serves the supported OpenAI endpoint without a pack or helper runtime.

Result: package SERVE-CMD owns tools/serve.f and the checked RUN ( SERVE-CMD:opts -- SERVE-CMD:run-result ) composition invoked after PARSE-ARGV by `bin/hb --load tools/serve.f --`. RUN selects one explicit INFER model arm, authenticates and loads the root, starts INFER, SCHED, and SERVE with the already-validated capacities, prints the bound address only after publication, and drives RUN-ONCE. Qwen requires batch one; a larger parsed value rejects before model or listener open. The once option writes the complete Content-Length response and executes ordered STOP. Every startup refusal returns or releases the exact owners acquired so far; every run or STOP refusal returns one terminal owner and error.

Add no argument grammar, streaming, pack, manifest, plugin, registry, auto-detection, download, host fallback, Python or shell server, JSON-line transport, daemonization, signal handler, schema, version, worker, default, or compatibility option. Owner: serve subsystem composition, loop, direct documentation, and process fixture only. Production red: parsed options cannot reach the real scheduler from a socket. Acceptance: fresh GPT-2 with batch greater than one and Qwen with batch one each serve one exact non-stream request through `/v1/completions`; invalid root, unsupported stream field, model-open failure, and every subsystem-start failure occur before or unwind listener publication as appropriate; once completion releases weights, session, cache, requests, sockets, and buffers; immediate restart succeeds. Smallest owning check: two direct once loopback commands, one per model. Claim: unassigned.
