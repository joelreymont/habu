---
title: "A forked child keeps PROCESS:STOP-FD after signal.f's reset"
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T15:08:30.059178+02:00"
---

Problem: once signal.f's fork-child reset (vnusvvpy) closes the stop pipe in a RAW child, PROCESS:STOP-FD still names its closed read end and PROCESS:STOP still points at a handler that calls SIGNAL:TAKE, which now throws E-SIGNAL-STATE, so a capture in such a child fails through STOP-HEARD (src/lib/process.f:670-673). No caller reaches it today (tools/check-core.f:1367 and the Tender and Maki runners set STOP-FD but never fork through RAW or SUBJECT:RUN). Acceptance: a RAW child of a process that set STOP-FD starts with no stop fd and runs a capture; a test shows it. Depends: none. Claim: unassigned.
