---
title: Own package state in neutral source replay
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T10:36:19.574081+02:00"
---

CG-24. Neutral start clears checker metadata (src/core/checker.f:524-535,623-631,10804-10807) but resolution (checker.f:5857-5875,6082-6095) still reads live engine package/using state, and src/habu/verify-source.f:732-769 does not replay source using/;using. A valid standalone dup definition returns 70 when the caller package owns DUP; source-declared using is also rejected although direct loading succeeds. Fix: replay owns one effective package+using state — neutral start initializes top-level/empty, source package and using events update it, replay resolution consumes it; ordinary compilation keeps consuming authenticated live engine state.

Scout update (2026-08-05): all cited content exists with small drift — CHECKER-PACKAGE-NEUTRAL family now checker.f:521-536, PKG-LIVE-DEFAULT/MIRROR-AUTHORITY :618-630, SCOPE-START-NEUTRAL :10810, CK-OPEN-CLAIMS? reading live engine cells :5863-5880. verify-source.f RECORD-DEFINER? :731-751 confirms the core claim exactly: no using/;using row anywhere in the file. Unverified: checker.f's second resolution site (cited :6082-6095) — re-locate at claim time.
