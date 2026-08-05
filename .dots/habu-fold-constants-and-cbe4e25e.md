---
title: Fold constants and number values on the IR
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T09:41:37.369599+02:00"
---

The chain has no redundancy elimination: after transitive inlining, repeated subexpressions and re-materialized literals survive to emission. Add to the frozen-IR pipeline (as a pass between elaboration and selection, verified by the existing freeze/canon machinery): constant folding with the source dialect's own trap discipline (a folded division by literal zero becomes the guard's refusal, never UB), algebraic simplification (x*1, x+0, x*2^n to shift), local-then-global value numbering over pure ops (the schema's PURE/TOTAL flags already say which), copy propagation, and the single highest-value special case — DIVISION BY CONSTANT to multiply-by-magic-number (derive the magic constants the standard way, prove them against the architecture division for the full input range in the test suite, both signed rounding directions). Acceptance: measured on the clang-column gaps — division-heavy rows close most of their gap; no answer moves anywhere; the pass is off-switchable for bisection and its output re-verifies under the module verifier.

Blocked by: habu-epic-hard-cut-a684f24d phases 1-6. Re-scoped: operate on typed frozen IR before selection; fold only operations whose trap/overflow/FP policy is explicit; start with repeated literals/subexpressions and constant division where wins are easiest to measure. Run DCE after inlining so copied unused values disappear.
