---
title: Open completion connection
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T07:03:53.810497+02:00"
blocks:
  - habu-own-checked-ipv4-44f29beb
  - habu-frame-bounded-http-d677ca95
  - habu-frame-completion-http-b2039e63
  - habu-infer-scheduler-req-1ac1dac6
  - habu-integrate-reentrant-json-34850f2f
---

Why: one accepted descriptor and its fixed storage need one owner before I/O or teardown behavior is added. Result: package SERVE-CONN defines one linear connection layout, OPEN, and package-private PREPARE-TOUCH. OPEN consumes a healthy SCHED scheduler, one checked nonblocking descriptor, caller-provided read, body, prompt, token-output, JSON writer state, JSON scratch, stable response, and HTTP write spans, canonical INFER:info, one server-minted response id, the server idle interval, and one accepted-at TIME:MONO-NS value. It derives SCHED:id and the first deadline with checked arithmetic, validates every span including JSON-WRITE:STORAGE-BYTES, initializes exactly one JSON-WRITE:writer, and publishes opened(scheduler,connection) only after all checks; refusal returns scheduler, descriptor, and every buffer owner. PREPARE-TOUCH consumes a connection plus one caller-captured monotonic time, preflights the next deadline from its stored interval, and returns prepared(connection,next-deadline) or refused(connection,error) without mutation. The opaque connection stores the scheduler id, model info, response id, idle interval, deadline, one writer, and at most one matching request handle. FOOTPRINT reports exact owned bytes. Owner: connection layout, OPEN, writer lifetime acquisition, deadline preflight, publication, and footprint only. Production red: no connection owner can retain explicit JSON state and scheduler binding, and no owner safely derives its idle deadline before irreversible I/O. Acceptance: exact and one-short spans, misaligned writer state, partial open, initial and refreshed deadline overflow, two schedulers, two connections, and every acquisition failure preserve owners; no caller scheduler id exists; two writers and prepared deadlines interleave. Forbidden: response-id minting, clock read, deadline commit, close, read, write, decode, render, submit, result apply, allocation, compatibility, metric, or lint. Smallest owning check: bin/hb --load maki/serve/connection-state-test.f.

Claim: unassigned.
