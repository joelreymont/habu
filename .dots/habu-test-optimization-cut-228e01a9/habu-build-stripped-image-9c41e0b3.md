---
title: Build stripped-image fixtures without the hb-build tool
status: closed
priority: 1
issue-type: task
created-at: "2026-09-29T18:01:21.013377+02:00"
closed-at: "2026-09-29T20:10:10.601288+02:00"
close-reason: Integrated stripped-image fixture consolidation; focused stripped suites and pooled native gate passed 510/510.
---

Problem: the stripped family (test/stripped-*.f, test/stripped-entry*.f, test/aot-image-class.f) makes 12 hb-build calls at ~18 s tool load each, uses inconsistent HABU_BUILD_CACHE roots, and several subjects only print and exit. Evidence and design: ~/.cache/tmp/kestrel-gate/test-review/L3-aot-image.md finding 4 (maker-direct preseed refusals as test/stripped-literal.f:50-66 does; merge print-and-exit subjects; FOLD aot-image-class). Acceptance: the same entry, preseed, refusal and run checks through the maker path or fewer merged builds; no assertion lost. Files/Ownership: test/stripped-*.f, test/stripped-entry*.f, test/aot-image-class.f, test/gate-stdlib-cases.f rows for them. Base: 614ae0ba (row-split stack head, not yet on master). Verify: every touched row passes standalone (bin/hb --load <row file>); a mutation of one moved or rewritten assertion fails; report per-row seconds before and after. Depends: none. Claim: agent=kestrel/worker workspace=.jj-ws/habu-build-stripped-image-9c41e0b3

Also (~/.cache/tmp/kestrel-gate/test-review/L2-compiler-other.md): FOLD aot-xt-cells (36 s) into the shared stripped build with stripped-does/literal/sparse-data/aot-image-class. Ownership adds test/compiler/aot-xt-cells.f.
