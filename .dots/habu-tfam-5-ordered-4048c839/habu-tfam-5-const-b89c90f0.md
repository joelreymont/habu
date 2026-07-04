---
title: "TFAM 5: constant logical-shape parity in replay/public-sig"
status: closed
priority: 2
issue-type: task
created-at: "\"2026-07-04T08:53:59.621169+02:00\""
closed-at: "2026-07-04T11:30:09.410710+02:00"
---

Stop hardcoding constant as -- a for layout values in verify-source.f (:381), tools/public-signatures-core.f (:552), and reconcile with all-errors which replays the literal 'N constant NAME' line (CA-ADD-SUPPORT-CONSTANT :357). Either reject layout constants consistently across all three replay paths or carry the full logical value shape. Add layout-producing constant fixtures proving none treat them as one-cell -- a. Part of item-5 replay parity; do via the shared event/replay framework.

## RCA / resolution (2026-07-04)

Static invariant: a `constant` may bake only a single physical cell, so a value
of a multi-cell layout family must not be silently narrowed to a one-cell `-- a`
signature. Sound enforcement point = the checker's `constant` value-pop
(interpret-mode stack width) — i.e. native C-CONSTANT, `src/habu/habu2.f`.

Evidence (fixture: `value-record cae-cv x i64 y i64 END-VALUE-RECORD`;
`TRUSTED: CAE-CV-MK ( -- cae-cv ) 0 ;`; `CAE-CV-MK constant CAE-CV-K`;
`: CAE-CV-USE ( -- cae-cv ) CAE-CV-K ;`):
- native `bin/hb --load` ACCEPTS `... constant CAE-CV-K` (exit 0) and narrows it
  to `-- a`; only the downstream layout USE is rejected (`inferred_effect -- a`
  vs `declared_effect -- field<cae-cv,x,i64> field<cae-cv,y,i64>`).
- verify-source preverify, all-errors (funnels through verify-source per
  LESSONS "all-errors support replay funnels through verify-source"), and
  public-signatures ALL model the constant identically as one-cell `-- a`.
- The only observable divergence is checker-vs-extractor: all-errors/verify-
  source (and native) fail-closed on the downstream layout USE (exit 70);
  public-signatures, a lexical extractor with no checker, publishes the same
  `-- a` trust and exits 0. No path invents a multi-cell shape.

Finding: the dot's premise that the paths diverge in how they MODEL the constant
is inaccurate — all four (native, verify-source, all-errors, public-signatures)
uniformly narrow to `-- a`. Neither "reject" nor "carry full shape" is soundly
achievable at item 5: (a) the value's layout width is knowable only by
interpret-mode stack tracking, which even native `constant` lacks today
(unbuilt items 6/12/15 — no layout-value constructors exist, so the reachable
case needs an artificial `TRUSTED: ( -- layout )` producer); (b) a lexical
layout-detector in the tools would be an unsound heuristic (forbidden), and (c)
rejecting in a tool ALONE would make preverify/all-errors stricter than native
(native accepts the constant), breaking native==redriver parity (LESSONS §43).

Resolution (item-5 scope): keep the uniform `-- a` model as a NAMED, tested
boundary; the sound rejection / shape-carrying is width-aware interpret-mode
work already owned by TFAM 12 (`habu-tfam-12-layout`, which lists
`constant/depth/.s` + interpret mode). Delivered:
- Boundary comments naming TFAM 12 at all three sites: verify-source
  RECORD-DEFINER? constant arm, public-signatures PS-MAYBE-TRUST-DEFINER, and
  all-errors CA-ADD-SUPPORT-CONSTANT.
- Parity fixtures locking that all paths narrow a layout constant to `-- a` and
  the checker paths fail-closed on the layout USE: `const-layout-narrow` in
  tools/check-all-errors-test.f (all-errors + verify-source funnel) and
  PST-TEST-CONST-LAYOUT in tools/public-signatures-test.f. Both flip when
  TFAM 12 makes `constant` reject/shape-carry.
Upgrade note for TFAM 12: when native `constant` rejects (or multi-cell shape-
carries) layout values, replace the `-- a` boundary in verify-source /
public-signatures accordingly and update both const-layout fixtures.
