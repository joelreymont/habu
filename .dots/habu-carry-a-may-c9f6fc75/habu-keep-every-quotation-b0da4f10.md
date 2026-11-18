---
title: Keep every quotation type written inside a quotation type
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T10:24:26.518755+01:00"
closed-at: "2026-11-18T11:24:41.907686+01:00"
---

Problem: src/core/checker.f KEEP-QUOTS* (:3294-3320) marks kept the quotations of a signature with no body only; whether a quotation type written inside a word's quotation parameter or output is kept is inferred by unification, and QUOT-PAIR (:4834-4837) reads it once, before a later tie can set it. `: W2R ( [ [ -- ] -- ] [ -- ] -- ) swap execute ;` called as `[: KV ! ;] [: EVC ;] W2R` (KV a `[ -- ]` cell, EVC rendering) is admitted and leaves EVC in KV; the other argument order is rejected. Acceptance: that program is rejected in either order, and a quotation type inside a quotation type states every fact of what it may hold. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-nB.
