---
title: Extent-role product/factorization capability
status: closed
priority: 2
issue-type: task
created-at: "2026-07-18T17:36:22.604554+02:00"
closed-at: "2026-07-20T12:59:58.354573+02:00"
close-reason: "Landed b192992e: BTC-7 per docs/batch-sequence-design.md section 5. Product former: built-in arity-2 extprod family rides existing parametric unification (ordered/mismatch rejects free); EXTPROD: #ROWS ( #FREE #INNER ) verifies value = free*inner (E-EXT-FACTOR -5064) and derives checked FOLD/SPLIT/JOIN. Contraction rule: redx<free-extent> and redx<whole-product> rejected at signature parse (checker.f EXT-REDX-BAD-ARG?) - the cross-sequence leak is now UNREPRESENTABLE, with a flat-baseline fixture proving it WAS representable before and disable-the-rule flips both rejects to accepts (red-first both directions). Engine text unchanged 136112/944 (rows already correct). FLAG: STATUS.md 'Certified: 987' has no census tool or gate assertion; the lane added 5 checked words (would be 992 if checker.f words count) - left unedited rather than write an unverified number"
---

Checker capability the (B,T,C) static guarantee depends on and Foundation A1 does NOT provide (A1 roles are flat): (a) product type former over declared extent roles so a folded row index can be typed as the product of B and T; (b) factorization/re-typing rule splitting it into free #B x in-block #T plus the inverse join; (c) contraction rule: a contraction accepts inner extents (#T, #k) and REJECTS a free extent (#B) - makes the cross-sequence leak unrepresentable; (d) lives in checker unification/role registry past A1 flat handling (coordinate habu-split-checker-f-837bc1a4 churn window) + candidate-B signature surface; (e) negative fixtures: mismatched factor, split whose factor product differs from the source extent, contraction over free #B rejected at load (exit 70). Hard prerequisite of the extent-roles and checker-reject dots. Full contract: docs/batch-sequence-design.md section 5 BTC-7.

Claim: agent=extprod workspace=.jj-ws/fable-extprod machine=spark (owns src/core/type-family.f checker.f role/signature surfaces + checker fixtures for the product capability; engine lane - CODELEN rows same-commit; coordinate note: habu-split-checker-f-837bc1a4 is unclaimed, no churn window active)
