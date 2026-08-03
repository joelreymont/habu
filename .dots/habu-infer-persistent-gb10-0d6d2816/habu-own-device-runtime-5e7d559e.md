---
title: Own persistent GPU session
status: closed
priority: 1
issue-type: task
created-at: "2026-07-30T00:55:48.211793+02:00"
closed-at: "2026-08-03T12:46:35.464323+02:00"
close-reason: Landed checked persistent GPU session cleanup and mandatory real-device proof at 41221c38.
blocks:
  - habu-delete-maki-cuda-22ba9ede
---

Problem: the production CUDA paths own resources only inside ambient CUDA-SCOPE frames, so no checked value can keep one context and stream alive across model calls. Result: add the properly packaged nominal CUDA:stream plus exact create, synchronize, and destroy driver bindings; extend the existing MKD injection seam for those calls and primary-context release; add maki/gpu-session.f reopening package GPU with one public DEFLINEAR GPU:session. GPU:OPEN ( -- result<GPU:session,n> ) allocates a private block, opens device 0, retains and sets its primary context, creates one nonblocking stream, and publishes only after every acquisition succeeds. GPU:CLOSE ( GPU:session -- result<n,n> ) consumes once, makes its context current, synchronizes and destroys the stream, unbinds the current context, releases the primary-context retain, attempts every cleanup step in reverse order even after a failure, frees the block, and returns ok(0) or err(first failing driver return code). Any OPEN failure unbinds and releases the acquired prefix, returns the primary operation's verbatim numeric code, and never lets a cleanup failure replace it. Owner: fixed persistent GPU driver lifetime only. Production red: GPU:OPEN is undefined; the exact packaged session/result declaration probe passes. Acceptance: the host injection matrix drives every acquisition and cleanup failure through GPU:OPEN/GPU:CLOSE, proves exact reverse once-only release, exact first-error reporting, and no leaked block, and proves two sessions coexist with distinct streams; the real DGX Spark path opens two sessions, closes either first, and leaves the other usable. No module, function, device buffer, target cache, or model state is allocated. Files: lib/ptx/cuda-driver.f, lib/ptx/cuda-driver-test.f, maki/cuda-run.f, maki/gpu-session.f, maki/gpu-session-test.f, and required existing suite rows only. Forbidden: DEVRT, generation, session id, registry, selector, plugin, raw public handle, callback scope, second CUDA ledger, compatibility alias, version, schema, manifest, lint, or unrelated migration. Smallest owning check: bin/hb --load maki/gpu-session-test.f on DGX Spark.

Reopened: independent review of 631fb8a found thrown driver failures can bypass cleanup and the real-CUDA leg is coupled to the host suite. The revision must return err(first), finish every cleanup step, and move the mandatory real proof to one standalone device test without skip logic.

Claim: agent=codex-gpu-session-r2 workspace=.jj-ws/habu-own-device-runtime-5e7d559e-r2
