---
title: Mirror the byte-exact local lookup in the seed
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-16T14:32:18.445549+03:00\""
---

Problem: src/habu/habu2.f EMIT-LOC-FIND (landed with 'Resolve a local only by its declared spelling') compares a reference to a live local byte for byte, but the Gforth recovery seed bootstrap/cg/forth.fs EMIT-LOC-FIND (around lines 3117-3143) still folds letters before comparing, so an engine recovered through the seed resolves locals by the old case-insensitive rule and disagrees with the native engine; test/compiler/native-local-case.f, test/local-spelling-suite.f and test/cast-suite.f would fail on such an engine. Acceptance: forth.fs EMIT-LOC-FIND mirrors the native routine: delete the two per-label lines for lname and ltoken, the six fold instructions (CMPI/BCOND pairs and the two ADDI $20) and the two label sites, and change the header comment from 'compare folded TKA/TKL with the locals table' to byte for byte; nothing else in the word moves; the hunk is identical in shape to the native diff. Verify: gforth test/bootstrap-engine-stack.fs; the stage0 chain tools/bootstrap.sh reaches 'bootstrap check OK'; test/local-spelling-suite.f and test/compiler/native-local-case.f (with test/compiler/aot-mode.f prefix) pass on the seeded engine. Files: bootstrap/cg/forth.fs. Depends: none (native side landed). Ownership: bootstrap/cg/forth.fs. Claim: agent=hazel-audit-seed workspace=.jj-ws/hazel-audit-seed.
