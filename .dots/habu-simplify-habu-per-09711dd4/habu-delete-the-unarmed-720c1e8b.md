---
title: Delete the unarmed tree shaker
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:30.757356+03:00"
---

Problem: src/habu/treeshake.f:69-84 KEEP? answers true unless SHAKE?, and SHAKE? is written only by test/engine-span-end.f:45,47, so production never shakes. The 95 KEEP? gates in habu2.f:10280-11637, FP-KEEP? habu1.f:87-123 and primitive-registry.f:187 always keep. The reachability half (treeshake.f:86-183) is test-only, and ADD-REACH :124-131 writes without a bound into a $10000 buffer. docs/architecture.md:142 describes the mechanism as live. Acceptance: treeshake.f and every load-list entry for it deleted; every KEEP?/FP-KEEP? gate replaced by its kept branch; test/engine-span-end.f keeps only checks unrelated to shaking (deleted if none); architecture.md:142 corrected; dot habu-move-evaluate-and-9119f746 no longer builds on KEEP?. Files: src/habu/treeshake.f, src/habu/habu1.f, src/habu/habu2.f, src/habu/primitive-registry.f, test/engine-span-end.f, docs/architecture.md, load lists naming treeshake.f. Verify: rebuild bin/hb with tools/native-build.f (docs/bootstrap.md); bin/hb --load test/run.f; tools/two-generation-build.f convergence (engine output changes). Depends: carl-final-core chain landed on master (it edits these files); start from master after it lands. Ownership: listed files. Claim: unassigned.
