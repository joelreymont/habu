---
title: Build completion poll plan
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T07:39:44.009921+02:00"
blocks:
  - habu-own-completion-srv-dfc8812e
---

Why: readiness interests and timeout selection are pure policy separate from the poll syscall and resulting transitions. Result: package-private SERVE:POLL-PLAN consumes and returns one server plus a bounded poll plan over its listener and fixed connection rows. It captures TIME:MONO-NS once, enables listener input only with a free row, selects read or write interest from each connection state, chooses timeout zero exactly when SCHED:RUNNABLE? is true and otherwise the checked duration to the nearest stored idle deadline, and writes no server or descriptor state. Owner: pre-poll clock capture, poll interests, and timeout calculation only. Production red: runnable inference can be hidden behind a socket wait. Acceptance: zero and full rows, every connection state, clock refusal, no deadlines, nearest deadline, expired deadline, duration narrowing, runnable with no readiness, exact poll capacity, and one-short capacity return exact server state. Forbidden: poll syscall, accept, connection transition, deadline refresh, scheduler tick, allocation, loop, retry, compatibility, metric, or lint. Smallest owning check: bin/hb --load maki/serve/server-poll-plan-test.f. Claim: unassigned.
