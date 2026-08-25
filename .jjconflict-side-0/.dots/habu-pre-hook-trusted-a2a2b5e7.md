---
title: "Pre-hook TRUSTED is silently inert: eight sites"
status: open
priority: 2
issue-type: task
created-at: "2026-08-20T16:31:49.676481+02:00"
---

VERIFIED (mark-1, 2026-08-20): a TRUSTED: definition compiled before the check hook exists registers NO checker row - ARENA-RC>PTR (checker.f:28) declares ( n -- ptr a ) yet answers CHECKER-RESOLVES? 0 and is DNAME-INT on master. Eight sites (7 checker.f, 1 check-hook.f). This is the deeper root cause behind the LOWER-CERT-HOOK:INSTALL axiom row and why seal-2 miscounted 18 for 19 (an inert TRUSTED: reads as an axiom in rg). Fix: the checker-boot registration path records pre-hook TRUSTED: declarations when the hook arms (the DRAIN-PRETRUST precedent at layout.f:866 replays name+effect for exactly this window - probe whether extending it covers the eight). 8-site blast radius, checker-boot change, own leaf.
