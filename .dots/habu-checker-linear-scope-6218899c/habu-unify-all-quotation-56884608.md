---
title: Unify all quotation throw rows
status: active
priority: 2
issue-type: task
created-at: "2026-07-26T20:48:45.151869+02:00"
---

Problem: THROW-EDGE records only the first catchable throw row in a quotation and silently ignores later throw rows. A later branch can therefore abandon or consume a different linear owner while the quotation metadata still reports the first safe edge. Required result: every catchable throw in one quotation shares one exceptional data row and return row. The first edge records them; every later edge must unify with both recorded rows and reject at that throw token on mismatch. Normal output and no-return metadata remain independent. Do not merge rows by erasing nominal or linear identity. Owner: quotation exceptional-edge accounting in src/core/checker.f. Dependencies: none. Acceptance: real checked fixtures with two straight-line/control-flow throw sites prove identical rows accept and different data or return rows reject; a hostile ordering fixture proves swapping which throw appears first cannot change the verdict; existing nonthrowing and single-throw quotation/catch suites remain green. Production seam: CHECK!/the normal bin/hb checker path, not a copied evaluator. Verify: focused quotation, return-stack, type-linear, and type-match suites.

Claim: agent=codex-throw-rows workspace=.jj-ws/habu-unify-all-quotation-56884608
