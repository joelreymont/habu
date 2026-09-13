---
title: Relocate the lowering-certificate arenas across a capture
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-13T10:03:56.285064+03:00\""
---

Problem: src/core/layout-valid.f (package LOWER-CERT) declares twelve per-definition arena bases as bare PTR-VARIABLE slots (PTR-VARIABLE X-P  X-BOOT X-P !: DATA-P, ENV-P, GUARD-OFF-P, TASK-KIND-P, FETCH-KEY-P and seven siblings) with no capture-seam reset, the same shape that LOC-HW-P had in src/core/checker.f; a PTR-VARIABLE is not in the relocation table, so a restored engine keeps the build window's address and writes lowering-certificate rows about 12 MB above its own heap top (measured 2026-09-13 in the fixed engine, heap top 10031582: DATA-P value-off 22732832 against boot-off 5533288, and likewise for the other eleven, all STALE above-here). They have not collided with an allotted cell yet, which is exactly how LOC-HW-P stayed latent until the two-generation chain died SIGSEGV in ASIG-STR-INTERN (habu-let-a-restored-1ba4b713). Acceptance: each slot becomes a PERSISTED-PTR-VARIABLE (or the arena family's relocated shape) with a snap-reset at the capture seam like LOC-HW-SNAP-RESET; a regression in the engine suite reads every one of the twelve out of a booted engine and asserts value = its boot buffer and cap = init; two same-host builds byte-identical; tools/two-generation-build.f gen 5 = gen 4; test/run.f unchanged. Files: src/core/layout-valid.f, src/core/checker.f (the capture seam), test/engine-suite.f. Verify: the regression red on the pre-fix engine and green after, the chain, test/run.f. Depends: none. Ownership: hazel. Claim: agent=hazel-worker workspace=.jj-ws/habu-let-a-restored-1ba4b713.
