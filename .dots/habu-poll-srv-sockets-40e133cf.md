---
title: Poll server sockets
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T01:12:14.601922+02:00"
blocks:
  - habu-build-completion-poll-e57ab507
  - habu-accept-ready-completion-a2d9d4bd
  - habu-drive-ready-completion-086b8540
---

Why: the completed poll plan, existing-row pass, and accept pass need one syscall composition. Interface: package-private SERVE:POLL-IO consumes one server, calls POLL-PLAN, invokes SOCK-OS:POLL exactly once, calls DRIVE-READY once for the rows captured by that plan, then calls ACCEPT-READY once only if the server remains healthy. Newly accepted rows therefore wait for the next poll plan. It returns the exact server or stop-only owner from those stages and performs no interest, timeout, accept, connection, or expiry logic itself. Owner: one bounded socket-poll composition only. Production red: the three ready phases have no product entry. Acceptance: instrumented production traces prove exact phase order, one poll call, no accept after stop-only, no same-iteration drive of a new row, and unchanged propagation of every syscall or transition refusal. Forbidden: copied phase logic, scheduler execution, result routing, general event loop, busy loop, thread, dynamic row, metrics, retry, version, or compatibility path. Smallest owning check: focused phase-order trace through SERVE:POLL-IO with real SOCK-OS descriptors. Claim: unassigned.
