---
title: "Report a source list's readable members under --all-errors"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T23:01:16.095969+01:00"
---

Problem: tools/check-core.f CHK-CHECK-INPUTS judges every input's open before the lints, so --all-errors --source-list t2.f locked.f prints only locked.f's E-UNREADABLE-SOURCE (rc 74) and none of t2.f's records; master prints none either. Acceptance: t2.f's records, then locked.f's at its turn in load order. Depends: none. Claim: unassigned.
