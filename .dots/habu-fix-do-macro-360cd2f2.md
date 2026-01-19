---
title: Fix do macro mapcar/mapcar2 dependency
status: open
priority: 2
issue-type: task
created-at: "2026-01-19T06:33:15.426855+02:00"
---

lib/stdlib.habu:875 - do macro uses mapcar #'car at line 876, mapcar2 at line 882. These are defined at lines 606-613. Macro expansion fails during stdlib load. Convert do/do* macros to use inline recursion or ensure mapcar/mapcar2 are available before use.
