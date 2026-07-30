---
title: Read completion request
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T07:03:54.105607+02:00"
blocks:
  - habu-open-completion-conn-497b7b44
  - habu-frame-bounded-http-d677ca95
---

Why: fragmented nonblocking HTTP input is independent from JSON decode, admission, expiry policy, and response writing. Result: SERVE-CONN:READ consumes one prepared reading connection, calls SOCK-OS:READ into its fixed read span, and advances HTTP-COMP parsing until would-block, one exact complete body, EOF, or refusal. Every positive read commits the already checked prepared deadline by a total store before publication; would-block discards the candidate and preserves the old deadline. Complete returns a body-ready connection whose body span is bounded and immutable until decode. It performs no clock read, deadline arithmetic, JSON operation, capacity calculation, scheduler match, submit, deadline comparison, or close. Owner: bounded request read, HTTP parse transition, and progress-triggered deadline commit only. Production red: an accepted connection cannot accumulate one fragmented request or refresh its owned deadline after progress. Acceptance: every split point, positive progress, would-block, early EOF, extra body, malformed framing, short read span, errno, and two interleaved connections select exact states, deadlines, and no overread; no deadline refusal exists after socket progress. Forbidden: decode, render, admission, scheduler, clock read, deadline preflight, expiry, close, response write, allocation, retry, compatibility, metric, or lint. Smallest owning check: real SOCK-OS fragmented reads through maki/serve/connection-io-test.f.

Claim: unassigned.
