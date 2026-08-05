---
title: Add corpus rows that exercise the missing transforms
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T17:02:22.168564+02:00"
---

The folding lane's naming gate proved the corpus supports only ONE of its five transform families: CALL-FAN-BIG carries 8 redundant literal materializations (constant-CSE, predicted 88->56 bytes), but SQUARE-SUM's a*a/b*b share nothing (not a CSE row), LERP is the only integer constant division and its byte gap is already ZERO, and no row exercises algebraic simplification (x*1, x+0, x*2^n), copy propagation, or general CSE. Per the binding rule — a transform no row exercises does not land — each future transform needs a measured row FIRST: engine answers pinned, clang twin in tools/clang/twins.c, per the existing corpus/twin shape (tools/codegen-compare-cases*.f). Choose rows that represent real habu code shapes (check maki/ and lib/ for actual patterns), including an integer constant division with real byte headroom. This dot is the gate for the remaining folding families and for any magic-divide lane.
