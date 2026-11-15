---
title: "Write TMPDIR's rejection as a record"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:34:21.168763+01:00"
closed-at: "2026-11-16T04:07:33.579900+01:00"
---

Problem: src/lib/fs-mutate.f:903-905 TMPDIR-REFUSE writes '-2103 habu: <path>: TMPDIR is not a directory' to fd 2 as a plain line, even under --json-errors. Acceptance: one record through REJECTION, prose or JSON as asked, printed only when the throw goes uncaught (kept with REJECTION:SAVE and thrown by PASS, carried by the throw on). Depends: habu-throw-on-a-289c8d47. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
