---
title: Drive ready completion sockets
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T07:39:44.317818+02:00"
blocks:
  - habu-own-completion-srv-dfc8812e
  - habu-drive-completion-req-39137097
---

Why: ready and expired existing rows need one bounded pass independent from accepting new descriptors. Result: package-private SERVE:DRIVE-READY consumes one server and the completed poll result, captures TIME:MONO-NS once after poll, visits each fixed existing row once, and calls SERVE-CONN:PREPARE-TOUCH with that same time before any matching ready transition. It passes only the prepared connection to SERVE-CONN:READABLE or WRITABLE; positive I/O progress commits the prepared deadline and would-block preserves the old one. After the transition it closes a row whose stored deadline is still expired. A cancellation failure returns the exact stop-only server with every unvisited row unchanged; clock, touch-preflight, transition, or close refusals preserve their named owners and perform no socket I/O after a failed preflight. It does not accept, poll, tick, or revisit a row. Owner: post-poll clock capture, touch-preflight trigger, and one ready and idle-expiry connection pass only. Production red: poll results cannot advance existing requests or safely refresh and enforce their one idle deadline. Acceptance: read and write progress, would-block, combined error readiness, clock refusal, no event, deadline edge, refresh overflow before I/O, timeout close, transition refusal, first and last cancellation failure, close failure, and three interleaved rows preserve exact state and visit counts. Forbidden: poll plan, poll syscall, accept, deadline formula or commit, scheduler tick, result routing, retry, dynamic row, compatibility, metric, or lint. Smallest owning check: real connection transitions driven by a completed SOCK-OS poll result. Claim: unassigned.
