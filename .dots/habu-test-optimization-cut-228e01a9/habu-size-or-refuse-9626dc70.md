---
title: Size or refuse a large checked-program capture
status: closed
priority: 2
issue-type: task
created-at: "2026-10-02T12:02:57.326190+02:00"
closed-at: "2026-10-15T01:26:45.252227+03:00"
close-reason: Completed or superseded on current master. b0e2b2aacca1 names truncated stdout/stderr capture bounds with exit70; original output cases failed before and passed after. Read-only source/history evidence checked in backlog review; no new code or broader runtime claim.
---

Problem (lane 284 repcap): tools/check-core.f:62-63 CHK-OUT-CAP ($8000) and CHK-ERR-CAP ($20000) bound the run capture. A checked program that prints 50 KB makes tools/check.f exit 67 with an uncaught -2504 (E-PROC-TRUNCATED) instead of a named refusal or the whole output. Acceptance: a fixture printing past the cap gets either its full output or a located refusal naming the cap, with check.f's documented exit code, never an uncaught throw. Files: tools/check-core.f, its test (tools/check-test.f).
