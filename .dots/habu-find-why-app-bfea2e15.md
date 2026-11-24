---
title: "Find why app-end.f's object is bad every build"
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T17:35:45.460189+01:00"
---

Problem: programs that share one .out mark src/habu/app-end.f's object BAD on every build (the reader stops at RD-AT 2369 or 2443), so it compiles each time; the cause is unknown. Acceptance: the cause is found and fixed at its root; a second warm build of two programs sharing an .out compiles nothing. Depends: none. Claim: unassigned.
