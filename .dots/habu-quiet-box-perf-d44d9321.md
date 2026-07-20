---
title: "Quiet-box perf re-certification of today's landings"
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T00:16:11.014233+02:00"
---

The XLA build is loading the box (load avg 20-28), so the declaration-event module (8763905f), the tied external golden + kernel-perf watch table (stack tip 24c19014), and any further landings until the box quiets were pushed on correctness-green + stable-sha evidence with the wall-clock perf band deferred (hard-fail attempts measured e=35274 and e=43924 vs budget 25250, correct=t, sha stable both runs - classic contention signature). When the box is quiet (load < ~5, XLA done): run the full cold gate once at master; if the perf band passes, close this dot with the measurement; if it hard-fails on a QUIET box, that is a real regression - bisect today's landings (declevents +44 text is the only src change in the deferred set) and treat as a blocker.
