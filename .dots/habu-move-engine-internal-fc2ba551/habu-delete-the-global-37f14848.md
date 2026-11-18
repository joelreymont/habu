---
title: Delete the global rows of private primitives
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T23:00:56.881520+01:00"
closed-at: "2026-11-18T13:22:51.349595+01:00"
---

Problem: 36 primitives with private rows (src/habu/prims.f EPPRIM:) keep a global EPRIM: row marked ETRUSTED-ONLY!, so a checked line outside the owner meets E-CAP-TRUSTED, or nothing at top level: `0 0 patch32` passes `tools/check.f -- --verify-only` (rc 0). A tick of a primitive whose only rows are other packages' private ones is admitted at check time, in a body (`: Y ( -- ) ['] xref-retarget drop ;`, rc 0) and at top level (`' xref-retarget drop`, rc 0), while its call is E-UNDEFINED. Acceptance: no primitive but ffi-call (the x86-64 step) has a global row; under `tools/check.f -- --verify-only` a call or tick, in a body or at top level, of a primitive outside the packages its private rows belong to is E-UNDEFINED at the token; ETRUSTED-ONLY! marks only ffi-call; the engine builds itself and the full suite passes. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-gr (rows), .jj-ws.noindex/jerry-tt (ticks).
