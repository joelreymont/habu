---
title: "Print a source frame's failed growth as a record"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T13:46:13.053140+01:00"
---

Problem: src/core/include.f GROW (:2046-2051) ends the process with the bare line 'include: cannot map a source frame' (INCLUDE-DIE, rc 74) when map-anon refuses a frame, where every other refused storage prints a rejection record (dynamic-storage.f MAP-REJECT's E-MAP). Acceptance: a refused frame growth prints one record naming the file being loaded and the bytes refused, with help, under prose and --json-errors; a test fails first. Depends: none. Claim: unassigned.
