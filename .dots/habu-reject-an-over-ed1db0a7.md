---
title: Reject an over-long command-line path with a record
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T02:09:12.722316+01:00"
---

Problem: for a command-line path over the cap, tools/check.f prints the plain line 'check.f: source path exceeds capacity' and exits 64, while bin/hb prints an E-PATH-RANGE record and exits 67. Acceptance: check.f prints the load's record and status for it. Depends: habu-campaign-c4-diagnostics-3b6de147. Claim: unassigned.
