---
title: Close completion runtime
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T07:49:13.241677+02:00"
blocks:
  - habu-parse-serve-cmd-07cb5e18
  - habu-infer-engine-owned-99a98d17
  - habu-infer-scheduler-req-1ac1dac6
  - habu-infer-dense-full-14833530
---

Why: model, engine, scheduler, and terminal owners need one reverse-order cleanup implementation shared by startup refusal and command shutdown. Result: package SERVE-CMD defines package-private CLOSE-MODEL, STOP-ENGINE, STOP-SCHED, and STOP-TERMINAL. CLOSE-MODEL consumes opts and model, calls INFER:CLOSE once, and returns closed(opts) or refused(opts,model,error). STOP-ENGINE consumes opts and engine, calls INFER:STOP once, and on success tail-calls CLOSE-MODEL with the returned model. STOP-SCHED and STOP-TERMINAL consume opts plus exactly their named owner, call SCHED:STOP once, and on success tail-call STOP-ENGINE with the returned engine. Every refusal returns opts, the exact latest live owner, and error without running a later step. No helper catches or retries. Owner: command inference-runtime cleanup chain only. Production red: startup and shutdown would otherwise duplicate different unwind orders or discard the owner returned by a successful stop stage. Acceptance: complete success and refusal at each scheduler, terminal, engine, and model stage preserve the exact latest owner; each successful prefix runs once in order; calling a helper with the wrong stage rejects through the production checker; two runtimes clean independently. Forbidden: startup, server, listener, memory release, loop, adapter, generic owner union, retry, version, compatibility, metric, or lint. Smallest owning check: bin/hb --load tools/serve-runtime-close-test.f through the real INFER and SCHED lifetimes. Claim: unassigned.
