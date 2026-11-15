---
title: "Check an app's MAIN at its definition"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T05:03:11.015169+01:00"
closed-at: "2026-11-16T04:07:36.616963+01:00"
---

Problem: an app build rejects a MAIN with the wrong effect at src/habu/app-end.f:5, never in the program; the user chose checking MAIN at its definition in app builds only, as rustc checks main. Acceptance: the build states the entry to the checker; a wrong MAIN is rejected at its declaration; a missing MAIN names the program; libraries are unchecked. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
