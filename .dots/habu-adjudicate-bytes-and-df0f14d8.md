---
title: Adjudicate bytes and widen the call rows
status: active
priority: 2
issue-type: task
created-at: "2026-08-04T19:11:56.565480+02:00"
---

From the adversarial benchmark review (probes preserved under /private/tmp/claude-501/bench-gaps): (1) tools/codegen-compare-report.f COSTLIER-ROW? adjudicates COST and ROW-MATCH? adjudicates outputs, but the SIZE columns are printed and never compared — 'rows the new column costs more on: none' is a time-only statement, which is how T-RES-WALK's 36-vs-76 byte loss sat green. Add BIGGER-ROW? and a 'rows the new column is BIGGER on:' line with the same finding discipline; on today's tree it must name T-RES-WALK (that row's fix is decomposed under habu-match-the-engine-1d6eb862). (2) corpus4's four callees are all ONE operation — the only size the engine's inliner copies (body 40 = INL-MAX exactly), so no committed row sees the call-site byte class: the chain pays 20 bytes/site (its in+out+3 interface) vs the engine's 4 (one bl). Add CALL-FAN-BIG: five call sites over a TWO-operation callee (probe6's FAN2/K2 bodies: measured old 36 bytes / 11.8 ns, new 96 bytes / 0.68 ns) — the smallest witness; the row will be RED on bytes until the residency+placement capabilities land (habu-keep-a-pass-8025401f, habu-place-the-data-9f128e58), which is the point. (3) Declare the refusal gap: seven values live across a non-inlined call inside a counted loop refuses E-A64RA-SPILL at the 6-to-7 boundary (probe3b L7; L6 is the largest accepted neighbour — old 516 bytes/159.3 ns, new 204/20.0) — a CODEGEN-GAP declaration so the coverage check keeps it named until it lands, plus optionally the L6 row measured. NOT wanted (measured and rejected by the review): a prediction-regime row (the byte column already guards conversion), a cold-entry row, a NaN-cost row.

Claim: agent=bench-rows workspace=.jj-ws/habu-adjudicate-bytes-and-df0f14d8
