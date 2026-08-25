---
title: tools delete candidates
status: open
priority: 3
issue-type: task
created-at: "2026-08-22T22:47:07.203513+02:00"
---

Problem: zero-consumer mechanisms measured by path reference: tools/diff-side-content.f/-read.f/-test.f and tools/lint/diff-frame.f, diff-frame-write.f, diff-path.f (no lint reads a framed artifact; both diff lints read raw jj diff --git via tools/lint/diff.f; one mention in src/core/checker.f to check); tools/data-residue-census.f, row-shape-probe.f, nested-validation-rca.f, unicode/class-generate-main.f, perf/boot-census-analyze.py (probes that answered once); tools/lint/sched-fixture/pretend.f (unreferenced fixture); tools/zed-run.f (dead host); 21 of 22 unsigned-decimal renderers (U$, U., EMIT-U, DEC., DDP-U$, RNL-U$, SL-U$, UB-U$, NL-U. ...) where FMT:.U exists. Acceptance: each deleted or its consumer named in the commit. Files: tools/. Verify: gate. Depends: none. Ownership: tooling. Claim: unassigned.
