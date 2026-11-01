---
title: "Let a tick of a fresh-input word take its argument's identity"
status: open
priority: 2
issue-type: task
created-at: "2026-11-01T08:13:37.870807+02:00"
---

Problem: `RR-OWN ['] RR-TAKE execute` with `RR-TAKE ( M<fresh-a> -- )` is refused while `RR-OWN RR-TAKE` is admitted (M = matrix<space-global,…,f32,f32>; test/rigid-region-suite.f words); likewise WF1T `RR-OWN ['] RR-WRAPF execute dup execute swap execute RR-UONE` refused, WF1 (direct) admitted, and the literal `RR-OWN [: RR-WRAPF ;] execute …`. Refused on master and every qmint fold (review /tmp/jerry-qmint-rv6-tmp, claim 2). forth.md: 'At a call, a fresh name on a plain input of the called word is the identity the argument carries', so the rule admits them. Cause: TICK-QUOT/EFF-QUOT instantiate the word's rows without E-AK-BIND, so E-I-AK mints the plain-input fresh name as a fixed identity at `[']` (a literal through E-AK-HOLD-NEW at its inner call); CHECKER-QUOT-FRESH:MARK leaves it unlisted (HOLDS?) and START/RENEW never renew it, so no argument fits. Proposed fix (reviewer): MARK records the identities HOLDS? keeps out; each execution (START/RENEW via QUOT-APPLY-INST) renews them to fresh variables the input unify binds to the argument's identity; the E-AK-CLAIM checks judge them, so two fresh inputs still need two allocations. Acceptance: suite rows `RR-OWN ['] RR-MFRESH execute drop` admitted and the C-CALLSAME analogue through a tick refused, each failing before; direct = tick for the rv6 probes. Files: src/core/checker.f (CHECKER-QUOT-FRESH, TICK-QUOT, E-AK-*), test/rigid-region-suite.f. Verify: rigid-region and linear rows, full suite. Depends: lane qmint landed (habu-mint-a-quotation-060811e6). Claim: unassigned.
