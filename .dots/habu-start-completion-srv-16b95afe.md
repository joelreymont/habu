---
title: Start completion server
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T07:31:05.561066+02:00"
blocks:
  - habu-start-completion-runtime-412475f9
  - habu-own-completion-srv-dfc8812e
---

Why: one started runtime and canonical storage plan must acquire storage and a listener before publication. Result: SERVE-CMD:START-SERVER consumes started(opts,scheduler,info), queries SCHED:RESULT-CAP, passes the four typed opts limits directly to SERVE:PLAN, narrows plan total through MEM:BYTES-ALLOC-LEN, acquires exactly one block through MEM:ALLOC-BYTES, opens the checked numeric IPv4 listener, and calls SERVE:OPEN with that exact pointer, allocation length, and requested idle interval. Success returns running(opts,server,bound-address), where bound-address is the checked value returned by SOCK-OS and is the sole value the command prints. On refusal it closes any acquired listener once, invokes the shared runtime cleanup chain, and only after runtime cleanup succeeds releases the exact memory pair through MEM:RELEASE-BYTES. Runtime cleanup refusal returns its terminal owner plus the memory pair; release failure propagates after device cleanup and produces no success. Owner: server storage acquisition, listener acquisition, bound-address publication, and SERVE publication only. Production red: a started scheduler cannot become a bound product server from the typed requested limits. Acceptance: every wrong-role PLAN call rejects statically; exact plan, size narrowing refusal, allocation refusal, address-in-use, partial OPEN, idle conversion refusal, each acquisition failure, runtime cleanup refusal before release, requested port zero and its actual bound address, successful release, and immediate restart preserve or release every owner exactly once; a real release failure emits no success marker. Forbidden: argument parsing, requested-limit construction, model open, capacity formula, server layout, poll loop, request handling, allocator implementation, caught release, retry, version, compatibility, metric, or lint. Smallest owning check: bin/hb --load tools/serve-start-test.f with real MEM, SOCK-OS, and SERVE. Claim: unassigned.
