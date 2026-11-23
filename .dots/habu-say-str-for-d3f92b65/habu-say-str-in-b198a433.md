---
title: Say str in the process argv and env words
status: closed
priority: 2
issue-type: task
created-at: "2026-11-22T17:00:09.314610+01:00"
closed-at: "2026-11-23T10:11:21.636230+01:00"
---

Problem: PROC-ARGV+, PROC-SPAWN-ARGV-ENV-IO, PROC-RUN-ARGV-ENV-IO-RC, RUN-ARGV-ENV-CAPTURE, RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME still take ^u8 len and FIND/RESOLVE-EXECUTABLE answer len, so callers add >LEN/LEN>N. Acceptance: those rows say str/n, no seam casts left; suites pass. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-land
