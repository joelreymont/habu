---
title: "Preserve generalized provider rows during call recording"
status: open
priority: 1
issue-type: task
created-at: "\"2026-09-13T11:45:43.535614+03:00\""
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: unassigned.

Own checker.f RECORDED-STEP/U-CALL-TAIL/CALL-FREEZE and row/quotation-copy regressions, excluding owner dispatch/payload sections. Freeze a fresh call instance without specializing published provider rows. p-only.f and v-quot-exec.f fail tier1; direct/e-only pass. Finally-only diagnosis is disproved. Preserve valid MEM effects, rollback and owner checks. Verify direct/quotation/finally across caller/provider tiers, empty/nonempty prefixes, wrong-type/borrow negatives, then WITH-CONTEXT-BOUND. Repro: .jj-ws/habu-keep-a-row-f2c4f3d4/build/repro.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.
