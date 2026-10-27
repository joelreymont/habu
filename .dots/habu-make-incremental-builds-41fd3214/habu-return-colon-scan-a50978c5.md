---
title: Return colon-scan results as values
status: closed
priority: 1
issue-type: task
created-at: "2026-10-26T13:24:06.665854+02:00"
closed-at: "2026-10-27T05:44:04.150925+02:00"
close-reason: "landed on master 9f905e31: full suite 503/503 (477 s), two-generation gen2..5 byte-equal, landing review ACCEPT (uzpuszmn)"
---

First earlier-write site CHECKER-COLON-N+0 (5 reports): CHECKER-COLON-SCAN passes results through globals. Return them on the stack; delete CHECKER-COLON-N/I. Test-first frame e2e case. Workspace: carl-colon, brief ~/.cache/tmp/carl-file/brief-colon.md.
