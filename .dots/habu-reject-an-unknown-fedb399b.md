---
title: Reject an unknown using in the declaration replay
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T15:43:16.597653+01:00"
---

Problem: OPEN-USING (src/core/checker.f, rg) does not reject E-USING-UNKNOWN at the `using` the replay reads, so the rejection lands later or nowhere. Acceptance: a replayed `using` of an unknown package is rejected E-USING-UNKNOWN at that token by the load and --verify-only. Depends: none. Claim: unassigned.
