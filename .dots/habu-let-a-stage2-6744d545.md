---
title: Let a stage2 engine boot without a captured AOT seed
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-12T13:24:28.452757+03:00\""
---

Problem: src/habu/habu2.f:5250 EM-SEED-AOT refuses LAOTNREC = 0 with 'hb: AOT metadata corrupt' (exit 82), while its own comment at habu2.f:5247 and the one at :9082 say a zero count skips the pass for stage2, maker and snap engines; LAOTNREC is baked from AOT-REC-N, which only aot-capture.f increments and tools/bootstrap.sh adds aot-capture.f only to the stdin driver, so every stage2 engine has N = 0 and the no-binary recovery chain stops at hb-stage with rc 82 (measured 2026-09-12 in .jj-ws/habu-recover-stage0 after the stage0 fixes). Acceptance: the code matches the documented rule (a zero count skips the seed pass for non-product engines while the product still refuses a missing seed by name), stated in one place, and the periodic no-binary check (docs/bootstrap.md) runs through hb-stage; a regression that builds a stage2-shaped engine and boots it. Files: src/habu/habu2.f, tools/bootstrap.sh, docs/bootstrap.md. Verify: the periodic check end to end. Depends: none. Ownership: hazel. Claim: agent=hazel-worker workspace=.jj-ws/habu-let-a-stage2-6744d545.
