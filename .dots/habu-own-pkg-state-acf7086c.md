---
title: Own package state in neutral source replay
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T10:36:19.574081+02:00"
---

CG-24. Neutral start clears checker metadata (src/core/checker.f:524-535,623-631,10804-10807) but resolution (checker.f:5857-5875,6082-6095) still reads live engine package/using state, and src/habu/verify-source.f:732-769 does not replay source using/;using. A valid standalone dup definition returns 70 when the caller package owns DUP; source-declared using is also rejected although direct loading succeeds. Fix: replay owns one effective package+using state — neutral start initializes top-level/empty, source package and using events update it, replay resolution consumes it; ordinary compilation keeps consuming authenticated live engine state.
