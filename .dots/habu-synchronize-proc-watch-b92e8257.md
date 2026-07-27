---
title: Synchronize proc-watch dead-path assert
status: active
priority: 1
issue-type: task
created-at: "2026-07-26T23:25:28.994601+02:00"
---

Why: pipe end-of-file proves descriptor teardown, not process death. The smoke
test can therefore open a pidfd before the child becomes waitable and fail its
zero-time assertion under load.

Owner: `PROC-WATCH-SMOKE` in `test/proc-watch-smoke.f`.

Behavior: hold the child on a go pipe. Open watch A while it is provably live,
release it, and wait for watch A to report kernel-observed death without
reaping. Open watch B on the surviving zombie and assert the already-dead
contract: immediate readiness on Linux and fail-closed behavior on macOS. Close
both watches, then reap. Preserve the live-child case.

Forbidden: sleeps, larger timeouts, retries, pipe EOF as the death signal,
`/proc` state polling, or reaping before watch B opens.

Acceptance: the focused smoke passes repeatedly beside competing load; the
standard-library and native gates pass; no timing constant changes.

Accepted implementation evidence:
`a5d1365518c53bf8c223d1428ce9c8fe8ee025b7` was independently reviewed and
accepted. The dot remains active until it lands and passes integration gates.

Claim: agent=codex workspace=.jj-ws/codex-proc-watch-dot
