---
title: "Drop the lint's code-name table"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T15:54:12.268837+01:00"
blocks:
  - habu-delete-or-run-47f6d22c
---

Problem: tools/lint/text.f:270-299 keeps an 8-slot code-name table (LINT-CODE-NAME+, dying when full or a name is long) that holds one row, EXIT-STATUS:FULL, so tools/lint/lib.f:45-49 LINT-THREW-NAME. can print its name. FULL is thrown by tools/lint/intern.f:77, :78 and :125 for an id or length out of range (runtime checks) and died with by tools/lint/token.f:38 for the fixed TMAX token table. Acceptance: the table and LINT-THREW-NAME. go, with LINT-MAIN, its buffer (lib.f:13-59) and the catch in tools/error-code-lint.f:22-24, so its MAIN calls ERROR-CODE-LINT:STRICT and hb reports an uncaught throw itself; intern.f's range checks go and the token table grows; the lints still run. Depends: habu-delete-or-run-47f6d22c (deleting the lints takes this with them). Claim: unassigned.
