---
title: Print the capture when a direct GT-RC@ reader times out
status: open
priority: 3
issue-type: task
created-at: "2026-10-01T17:17:49.777299+02:00"
---

Problem: rows that read GT-RC@ directly rather than through GE-RC@ (test/gate-common-lib.f, wb-geexp) print no capture when the entry ran out of time: GT-RC@ throws E-PROC-TIMEOUT before any report. Sites: test/outer-interpret.f:141 and :154 (KEEP, SAME), test/outer-number.f:38, test/compiler/native-div-refusal.f:211 and :220. Acceptance: each site reads its status through GE-RC@ (or the equivalent that prints the capture first) so a timed-out entry prints its stdout/stderr and then throws E-PROC-TIMEOUT; a forced-timeout case shows the capture before and not after; no other behaviour changes. Files: the four test files. Verify: each row alone rc 0; the forced-timeout probe.
