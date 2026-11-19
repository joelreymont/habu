---
title: "Ground RAN-TIED?'s trail look-back"
status: active
priority: 2
issue-type: task
created-at: "2026-11-19T16:51:42.540147+01:00"
---

Problem: RAN-TIED? (src/core/checker.f, beside RAN-KEPT?) looks back at most three trail entries from a run record and answers not tied at the first entry outside the run's group; its comment says one run's four records sit together on the trail since no path compression runs within QUOT-INST. But R-RES compresses (R-COMPRESS writes TRAIL-LINK tag 13) whenever COMPRESS? holds (TRIAL-DEPTH and HOLD-N both 0), RSEXEC calls QUOT-APPLY-INST at a plain step, and QAPP-REG's ROW-TAIL calls R-RES between run records (a merge review of the loop lane found this; it could not show a two-link chain there). Acceptance: either a program puts a compression entry between one run's records and the check no longer depends on the records being adjacent, with that program as a test; or the comment states why no such entry can occur, citing what prevents it. Depends: the loop lane (skmulorv, workspace .jj-ws.noindex/carl-anyq-d) landing. Claim: agent=worker workspace=.jj-ws.noindex/carl-rantied.
