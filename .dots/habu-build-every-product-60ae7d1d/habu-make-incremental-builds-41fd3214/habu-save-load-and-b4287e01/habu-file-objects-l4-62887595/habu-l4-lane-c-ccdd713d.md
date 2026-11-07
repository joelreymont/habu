---
title: "L4 lane C: Link an object's code entries"
status: active
priority: 2
issue-type: task
created-at: "2026-11-07T01:29:13.885176+02:00"
---

Problem: L4 needs LINK-DEF and NEMIT-FILL in linker.f (a definition's record via OUTER:DEF-RECORD, its shadow emission via NEMIT's fill words and NSHADOW:TAKE/TAKE-DOES/SOURCE-FUNS/PUBLISH, its host copy via ARM64:PLACE then NPUB:IMPORT-PENDING, -DOES or -DEFER, flags via SET-FLAGS); OUTER:DEF-CLOSE goes public. Brief .jj-ws.noindex/carl-l4lc/.out/brief-l4-code.md. Verify: the four suites (TFAIL = carl-l4/.out/l4i-suites.log) and a read-only probe comparing NEMIT-FILL's emission with the compile's shadow emission, plain and second builds, 0 mismatches. Acceptance: the lane's brief items done as written, a code-reviewer ACCEPT, merged into .jj-ws.noindex/carl-l4. Depends: lane R (ksuswurv). Ownership: worker lane. Claim: agent=worker workspace=.jj-ws.noindex/carl-l4lc (worker done; review REVISE: cleanup on a throw; lead fold: LINK-DEF catches MAKE-DEF, then NEMIT:CLEAR, NSHADOW:ABANDON, OUTER:DEF-ABORT (public), LINK-CLEAR, rethrow; merged into carl-l4 rwyzklot; fold re-review ACCEPT, probe of 7 throw points clean)
