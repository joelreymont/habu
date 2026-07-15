---
title: "Migrate Maki string callers to STR:"
status: active
priority: 3
issue-type: task
created-at: "\"2026-07-15T15:05:05.427807+02:00\""
---

Full context: MODEL-CAD-V2-PLAN.md B5.5a legacy-STR census, Maki lane. Migrate raw STR calls in: maki/store.f, store-rehydrate.f, competitive-store.f, cad.f, eval-transcript.f, golden-artifact.f, eval-repair-loop.f, ablate-ptx.f (FIND-SUB/INDEX-OF/SPLIT-NEXT per census), and the FIND-SUB uses in maki/lower-red-test.f, lower-mm-test.f, lower-ew-test.f, lower-mv-test.f, onnx/deploy-test.f. No BUF use. Overlap note: cad.f/golden-artifact.f touched by the landed Model-IR wave (sequential). Acceptance: fresh rg census empty; maki/test.f + focused tests green. Files: the 13 listed + focused tests. Verify: maki suite. Ownership: the 13 Maki files. NOTE: lower-*-test.f are TEST files, distinct from sol's lower-*.f - still confirm no active claim at dispatch.

Claim: agent=makistr workspace=.jj-ws/fable-effseal
