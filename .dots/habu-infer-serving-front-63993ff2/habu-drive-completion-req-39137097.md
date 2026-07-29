---
title: Drive completion request I/O
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T01:12:14.348645+02:00"
blocks:
  - habu-infer-serve-http-4fb09e9a
  - habu-infer-serve-openai-1dca13cd
  - habu-render-completion-json-9fff2d34
  - habu-infer-scheduler-req-1ac1dac6
---

Why: connection storage lifetime is separate from request read, decode, submit, and response write transitions. Interface: SERVE-CONN:READABLE consumes one opaque connection plus the healthy scheduler, performs bounded reads to would-block or one complete body, decodes once, proves OPENAI-COMP:RESPONSE-BOUND fits the connection JSON buffer and max_tokens times model MAX-TOKEN-BYTES fits output storage, then calls SCHED:MATCH-ID with the connection's stored id before SCHED:SUBMIT. Identity mismatch returns refused(scheduler,connection,cross-scheduler) before admission and installs no handle. A match submits through that same scheduler and stores only its returned matching request handle. WRITABLE frames the stable JSON body and resumes partial writes to would-block or close. EOF, malformed input, timeout, submit refusal, and close return named states with exact owners. Owner: one-request nonblocking connection I/O transitions and submit-time scheduler authentication only. Production red: a connection can otherwise install a handle from a scheduler different from the one that supplied its teardown identity. Acceptance: every read split, short write, would-block, early EOF, malformed or oversized request, capacity refusal before SUBMIT, timeout, and close failure follow exact states through the real HTTP and scheduler path; swapping two authentic schedulers against two bound idle connections rejects before SUBMIT with both owners intact, while each matching pair installs its own handle. Forbidden: caller-supplied identity, state allocation, result application, scheduler tick, second request, heap buffer, retry, blocking I/O, task, callback, version, or compatibility route. Smallest owning check: focused loopback request-I/O traces with the real scheduler.
