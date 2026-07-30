---
title: Serve completions from one command
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:44.376529+02:00"
blocks:
  - habu-drive-completion-cmd-b4ca1b5d
  - habu-prove-concurrent-completion-ac1a26ca
---

Why: the engine is not a usable vLLM replacement until one native command composes the completed product lifetimes without a pack or helper runtime.

Result: package SERVE-CMD owns tools/serve.f and checked RUN ( SERVE-CMD:opts -- SERVE-CMD:run-result ), invoked after PARSE-ARGV by `bin/hb --load tools/serve.f --`. RUN calls OPEN-MODEL, START-RUNTIME, START-SERVER, then DRIVE exactly once in that order and adds no subsystem logic. It prints the checked bound-address returned by START-SERVER only after publication and before DRIVE. Every result arm is the exact result of the owning stage.

Add no argument grammar, model constant, capacity calculation, cleanup copy, streaming, pack, manifest, plugin, registry, auto-detection, download, host fallback, Python or shell server, JSON-line transport, daemonization, signal handler, schema, version, worker, default, compatibility, metric, or lint. Owner: final command composition, direct documentation, and two process fixtures only. Production red: the completed lifetime stages have no native command entry. Acceptance: fresh GPT-2 with batch greater than one and Qwen with batch one each serve one exact non-stream request through `/v1/completions` under --once; each stage refusal is returned unchanged; each success releases weights, session, cache, requests, sockets, writers, and memory; immediate restart succeeds. Smallest owning check: two direct once loopback commands, one per model. Claim: unassigned.
