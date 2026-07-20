---
title: Extent-role product/factorization capability
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-18T17:36:22.604554+02:00\""
---

Checker capability the (B,T,C) static guarantee depends on and Foundation A1 does NOT provide (A1 roles are flat): (a) product type former over declared extent roles so a folded row index can be typed as the product of B and T; (b) factorization/re-typing rule splitting it into free #B x in-block #T plus the inverse join; (c) contraction rule: a contraction accepts inner extents (#T, #k) and REJECTS a free extent (#B) - makes the cross-sequence leak unrepresentable; (d) lives in checker unification/role registry past A1 flat handling (coordinate habu-split-checker-f-837bc1a4 churn window) + candidate-B signature surface; (e) negative fixtures: mismatched factor, split whose factor product differs from the source extent, contraction over free #B rejected at load (exit 70). Hard prerequisite of the extent-roles and checker-reject dots. Full contract: docs/batch-sequence-design.md section 5 BTC-7.

Claim: agent=extprod workspace=.jj-ws/fable-extprod machine=spark (owns src/core/type-family.f checker.f role/signature surfaces + checker fixtures for the product capability; engine lane - CODELEN rows same-commit; coordinate note: habu-split-checker-f-837bc1a4 is unclaimed, no churn window active)
