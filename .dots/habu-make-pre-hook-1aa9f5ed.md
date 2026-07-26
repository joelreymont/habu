---
title: Make pre-hook core throw codes nameable
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T11:41:33.920474+02:00"
---

Checker-visibility gap found by the MODELPROV lane 2026-07-26: error constants defined in the first core prefix file (example: the new E-PATH-RANGE 7134 in src/core/util.f) load before the check hook installs, so checked user code cannot name them and suites are forced to pin raw ABI numbers with a comment (the MODELPROV suite pins 7134 that way today; it fails closed if core renumbers, but a number is still a value where a name is possible). Behavior: make pre-hook core throw-code constants nameable from checked code - either re-export them through a checked core surface after the hook installs, or teach the certification hook about the pre-hook constant vocabulary; whichever is chosen must not weaken the hook (a pre-hook WORD must not become callable, only the named constants). Acceptance: a checked fixture names E-PATH-RANGE (and one other pre-hook code) and certifies; the MODELPROV suite retires its raw 7134 pin; a hostile fixture proves a pre-hook non-constant word still refuses. Owner: the check-hook installation seam in src/core. Dependencies: lands after the MODELPROV leaf merges (the 7134 pin is the live example to retire).
