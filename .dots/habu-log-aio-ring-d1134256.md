---
title: Log AIO ring setup failures
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T15:34:08.733145+01:00"
---

Problem: src/lib/aio.f:743,745,747 throw E-AIO-SETUP for a refused ring mapping with no text or log line, while card section 6 has the library that saw an outside failure log it once. Acceptance: each refusal logs one aio line with the OS's text before the throw. Depends: none. Claim: unassigned.
