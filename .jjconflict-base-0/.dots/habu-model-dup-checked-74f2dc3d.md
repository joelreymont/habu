---
title: Model ?dup checked union effect (scalar arity)
status: open
priority: 2
issue-type: task
created-at: "2026-07-05T00:04:08.142352+02:00"
---

Static invariant: a checked word using ?dup must have its post-?dup stack shape verified by the checker before runtime; the boundary is the ?dup token model in src/core/checker.f. Current gap: ?dup is unmodeled in the checker — it is not a PRIM:, not in CF-TOK?, and not in RS-TOK?, so it falls through DO-TOK to the undefined path and sets UNCK=1 (verdict 'uncheckable'). Evidence: probe P-QDUP-N (scalar '?dup drop drop') returns verdict 1. Item 12 slice 1 (habu-tfam-12) added QDUP-STEP? which REJECTS ?dup on a layout value (fail-closed, width-breaking on the tag cell) but deliberately leaves the scalar case UNCK unchanged (pre-existing). This is an unchecked escape hatch: any checked word using ?dup on a scalar is uncheckable. Fix: model ?dup's value-dependent union effect (x -- x x | x -- x), i.e. the checked 'dup if ... then' idiom, so scalar ?dup usage is fully verified. Then flip QDUP-STEP?'s scalar branch from '-1 UNCK !' to the real effect and keep the layout rejection. Add positive scalar fixtures (?dup if) and keep the layout-reject negative in test/type-decl-suite.f.
