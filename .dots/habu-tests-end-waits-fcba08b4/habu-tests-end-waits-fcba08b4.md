---
title: Tests end waits on another party by a clock
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T19:40:02.668948+01:00"
---

Problem: tests end waits on a task, a process or a server by a deadline or a count: TEST-WAIT:POLL deadlines, TEST-WAIT:EXPIRED after a hand-rolled mono-ns deadline or count, and process captures given a timeout, in about 90 test files. Under load such a deadline fails a passing test, as curl-test WAIT-READY did in the full suite. test/AGENTS.md: a hang is named by its WAIT line, never ended by a clock; a test that sets a deadline of its own and expects it asserts it. Plan: a child dot per slice; a library wait takes ms -1 as no deadline, as PROCESS:POLL does. After the TEST-WAIT:POLL child lands, slices for the other rows of its callers' files (http, curl, bearer, test-issuer, mcp, model, aio, signal, task, crew, process-command, process-task, process-pty-io-smoke, http-accept-starved); after the FIFO child, gate-signal-test and check-signal-test; then fs-rename-noreplace-test's race wait, and TEST-WAIT:EXPIRED goes if nothing calls it. Acceptance: no wait on another party has a deadline; each deadline left is one its case asserts. Depends: none. Claim: unassigned.
