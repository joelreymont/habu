---
title: tests whose subject is the harness
status: open
priority: 3
issue-type: task
created-at: "2026-08-22T22:47:07.078377+02:00"
---

Problem: test/gate-stdlib-cases.f:1436-1463 STDLIB-GATE-TEST:RUN asserts wordlist membership of MAIN/SKIP-SEMANTIC! in the gate adapter on every test/gate-stdlib.f run; test/engine-suite.f:643-651 'candidate dictionary/hook smoke' asserts one baked word resolves (deleting the process subsystem's behaviour would not flip it). Acceptance: both replaced by a behaviour assertion or deleted. Files: as listed. Verify: gate green. Depends: none. Ownership: test harness. Claim: unassigned.
