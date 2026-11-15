---
title: "Fix the xmodem tests' bare READ-ALL"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T13:30:07.285804+01:00"
closed-at: "2026-11-16T04:07:36.102384+01:00"
---

Problem: test/xmodem.py and test/serial-xmodem.py SETUP call bare READ-ALL, a public of package FS, so every case they build is rejected with E-UNDEFINED, and serial-xmodem.py's rejected-signature checks see that record. Acceptance: both run green and serial-xmodem.py's rejected-signature checks pin the whole record they print. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
