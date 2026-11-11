---
title: process.f throws E-PROC-OUTPUT without logging
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T18:53:05.633229+02:00"
---

Problem: src/lib/process.f throws E-PROC-OUTPUT with no log line; docs/forth-card.md section 6 says a thrown error is logged where it is raised (POLL review, 2026-11-11). Acceptance: each E-PROC-OUTPUT throw site logs what failed before it throws, per the card. Files: src/lib/process.f. Verify: src/lib/process-test.f rc 0 and the log line read at one site. Depends: none. Ownership: unassigned. Claim: unassigned.
