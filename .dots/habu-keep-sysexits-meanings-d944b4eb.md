---
title: "Keep sysexits meanings for Habu's exit statuses"
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T10:13:43.953587+01:00"
---

Problem: src/core/exit-status.f reuses sysexits(3) numbers for other meanings: 78 SYSTEM is EX_CONFIG, 75 IMAGE-BOUNDS EX_TEMPFAIL, and 67, 72, 73, 76 and 77 likewise. An uncaught E-MEM-MAP exits 78 (src/lib/error.f STATUS), so Maki's unit (RestartPreventExitStatus=65 75 78) never restarts after a refused mapping (tim, /tmp/tim-habu-exit78.md, via carl). Acceptance: a status from 64 to 78 means what sysexits says (SYSTEM is 71, EX_OSERR); UNCAUGHT, ENCODE, IMAGE, IMAGE-BOUNDS, INTERNAL and FULL take 85-90; every test, peer and doc that names a moved number follows; tim is told before the install. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-exit.
