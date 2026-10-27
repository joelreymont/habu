---
title: Find why compiler-images-build throws E-MEM-SIZE
status: active
priority: 2
issue-type: task
created-at: "2026-10-27T11:04:05.774774+02:00"
---

test/compiler-images-build.f exits 67 with uncaught -3200 (E-MEM-SIZE) after 'compiled 14052' (tim, base f89067a1; log ~/.cache/tmp/tim/l2b/cimg-base/log). Passed Oct 26 on tim's cf9b3b66. Root-cause lane in workspace .jj-ws.noindex/carl-cimg.

Root cause (CIMG worker + carl, 2026-10-27): no master defect. The reproducer omitted HABU_GATE_COMPILER_BUNDLE=1, which every real caller sets (test/compiler-images.f:28, test/native-layout-transition.f:52). Without it internal-mark.f IMK-PASS takes IMK-SEAL, no seal plan is recorded, and COPY-SEAL-PLAN (tools/native-build-core.f:696) allocates 0 cells -> E-MEM-SIZE after a full build. With the switch: rc 0 (119 s). Fix (carl-cimg, "Refuse compiler images without the bundle switch"): test/compiler-images-build.f refuses at entry with rc 72 "compiler bundle switch missing", as test/gate-images-orch-builder.f:59 does. Proof: no switch rc 72 at once; switch rc 0 235 s. Lands with the next batch.
