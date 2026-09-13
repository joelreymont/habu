---
title: Build the compiler with the optimizer (selfbuild)
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-11T16:38:06.278925+03:00\""
---

Problem: the cold seed loads compiler.f before NCOMP:COMPILE exists, so the shipped compiler body is JIT code (IR-ID:COUNT-N opens with sub sp,sp,#16); every pass pays JIT frame-call overhead. Acceptance: a stage-2 engine whose compiler is compiled by the stage-1 optimizer, verified by the leaf disassembly probe and the span table reporting 0; the forced-tier Tender load measured on it as the next-generation baseline. Files: tools/native-build.f, src/habu/native-runtime.f, bootstrap. Verify: forced-tier Tender load on the stage-2 binary. Depends: none. Ownership: cedar (workspace .jj-ws/cedar-aot-selfbuild). Claim: unassigned


Current ownership and handoff: Owner: cedar. Current checkpoint 114c1c09c9c729c052fcc7a84763dbed2ecce424051857ecff04305dc675289b supplies new layout and dispatch. Old optimizing host failed BSETTIER at undefined NCOMP-DISPATCH:TIER-CELL after 38.161 s; replaying baked layout is invalid. Correct transition: temporary cold current checkpoint, fresh process with test/compiler/aot-mode.f before tools/native-build.f. Actual optimizing selfbuild now running, log /tmp/cedar-current-optimizing-selfbuild.log. No completed optimizing output claimed.

Parked 2026-09-13 (hazel, account limit): stack of 30 commits rebased on 78d5af92 in .jj-ws/rowan-tier, top 91fff115 (record extension with 38 front-end operations and two regimes, int-mark/min-in-mark effect rows, CHECK-UNJUDGED on the record, checker-owner lint, tape-owner tests). Product rebuild advances into src/compiler and stops at WITH-CONTEXT-BOUND on habu-keep-a-row-f2c4f3d4 (a root defect). Still to do: p2-map-rewind's two inliner-era cases retired with reason, compiler suites with test/compiler/aot-mode.f, run.f by name vs the recorded 7, coherent squashes, product rebuild and chain after the row fix, then close with habu-retire-the-pre-a37792de's arms documented.
