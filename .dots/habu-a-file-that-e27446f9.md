---
title: A file that makes the namespace wordlist current exits 83 with no message
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T21:01:24.051438+02:00"
---

Problem: a file whose first line is 'BOOT:NAMESPACE-WL set-current' followed by ': MAIN ( -- ) ;' ends hb-build and tools/check.f with exit code 83 (reproducer: those two lines as ns.f, 'hb --load tools/check.f -- ns.f'). The one line it prints names no token: 'E-SEAL-VIOLATION habu: def-open with a bad kind, a marker wid, a definition pending, CP at the code ceiling, no record room, an empty name or one reaching the code ceiling, or a name taken lr <addr>'. Cause not looked for. Acceptance: the refusal names the token and why, as other refusals do. Depends: none. Claim: unassigned.
