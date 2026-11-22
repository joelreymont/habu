---
title: Run the Python peer tests in the suite
status: open
priority: 2
issue-type: task
created-at: "2026-11-22T16:59:16.718214+01:00"
---

Problem: test/serial.py, test/xmodem.py and test/serial-xmodem.py run in no suite (test/gate-*-cases.f names none), so serial.py kept five expectations that 890f82e8 and 6168148 made stale. Acceptance: a suite runs the three with the candidate engine and fails when any fails; the full suite passes. Depends: the process argv str lane (jerry-p3proc). Claim: unassigned.
