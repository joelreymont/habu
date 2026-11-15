---
title: Settle one identity or one per execution for a body-supplied fresh name
status: open
priority: 2
issue-type: task
created-at: "2026-11-01T08:13:37.880280+02:00"
---

Problem: a callback whose quotation a body supplies with a fresh name holds one identity per body, so it cannot be forwarded to two calls: FG6 `( [ [ -- M<fresh-a> ] -- x y ] -- x y ) dup RR-GIVEP 2drop RR-GIVEP`, RC10 (the same through RECURSE) and FG8 `[: RR-OWN ;] over execute drop [: RR-OWN ;] swap execute` are refused on master and every qmint fold. docs/forth.md disagrees with itself: ~1401 'must end holding a region this body minted' reads as one identity per body, while the per-execution paragraph (~1426, lane qmint) says each execution of that handed quotation mints. Cause: the one-binding exemption in TV! (FB-VAR?) then FB-OWNED?/CHECKER-QUOT-FRESH:FITS?. Acceptance: one statement in forth.md and the card that the code implements; FG6, RC10 and FG8 answer by it with suite rows failing before if the code changes. Files: src/core/checker.f (TV!, FB-OWNED?, CHECKER-QUOT-FRESH), docs/forth.md ~1399-1440, docs/forth-card.md ~168-170, test/rigid-region-suite.f. Depends: lane qmint landed (habu-mint-a-quotation-060811e6). Claim: unassigned.
