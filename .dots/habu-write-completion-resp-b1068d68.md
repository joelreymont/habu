---
title: Write completion response
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T07:03:54.542376+02:00"
blocks:
  - habu-apply-completion-result-021d8f78
  - habu-frame-completion-http-b2039e63
  - habu-close-completion-conn-7b2fdd67
---

Why: a stable rendered body needs one resumable nonblocking HTTP write transition independent from request admission. Result: SERVE-CONN:WRITE consumes one prepared writable connection, frames its stable JSON body through HTTP-COMP, resumes SOCK-OS:WRITE from the stored cursor to would-block or completion, commits the already checked prepared deadline by a total store after every positive partial write, and closes through the owning connection close path after completion. Would-block discards the candidate and preserves the old deadline. It never reads the clock, performs deadline arithmetic, rerenders, mutates scheduler state, or retries a consumed descriptor. Owner: response framing cursor, socket write transition, and progress-triggered deadline commit only. Dependencies: connection result application, HTTP framing, and healthy close. Production red: a completed response cannot reach a socket across partial writes while retaining one owned idle deadline. Acceptance: every short-write split, progress refresh, would-block, exact Content-Length response, errno, close failure, and two interleaved writable connections preserve exact cursors, deadlines, and owners; body bytes never change and no deadline refusal exists after socket progress. Forbidden: read, decode, submit, render, clock read, deadline preflight, expiry, scheduler tick, allocation, busy loop, compatibility, metric, or lint. Smallest owning check: real SOCK-OS partial writes through maki/serve/connection-io-test.f.

Claim: unassigned.
