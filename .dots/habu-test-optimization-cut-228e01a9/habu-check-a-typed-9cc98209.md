---
title: "Check a typed cell's quotation type at xt!"
status: closed
priority: 3
issue-type: task
created-at: "2026-10-04T02:34:47.707006+03:00"
closed-at: "2026-10-22T12:36:34.603543+03:00"
close-reason: "Triage 2026-10-22, DONE: probe xt.f: xt! of a mismatched quotation into a typed cell is refused E-MISMATCH exactly as ! is (xt2.f)."
---

Problem: the checker applies the typed-cell store contract (src/core/checker.f CELL-STORE-TOK / STORE-QUOT-CONTRACT, ~:14180 on d37500ab) to ! only, so `TYPED-VARIABLE QA [ a -- a ] : QS2 ( -- ) [: 1 + ;] QA xt! ;` certifies on both the d37500ab engine and lane 556's (dot 9fc427a9) engine, while the same store through ! is refused. Found by lane 556 ($HOME/.cache/tmp/kestrel-jerry-xtkind/HANDOFF.md). Fix: xt! into a typed cell meets the same contract as !, refusing a quotation whose effect differs from the cell's declared one. Acceptance: QS2 refused with the reason ! gives (seen certifying first); a matching quotation stored with xt! into QA certifies and runs; baked: rebuild, g1 == g2, two-generation build. After: 9fc427a9.
