---
title: "Primitive facts text comes from the kernel object's rows"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T10:34:54.953226+02:00"
---

Problem: privacy step 2d (privacy-design.md Design 1): src/habu/linker.f HB-OWNED finds an owned primitive's (prims.f EPPRIM:, kind K-PKG-PRIVATE) facts text by name in the host's private wordlist, which a stripped host lacks, and src/core/check-hook.f OWN-OR-ROWS returns that host record's text first. Step 2c (dot habu-a-build-s-87c9c7c2) takes the global primitives: PRIM-TEXT and PRIMITIVE-FACTS render from the rows the build stated, and its replay (link-kernel.f STATE-PRIMS) skips K-PKG-PRIVATE rows. Acceptance: the replay states the K-PKG-PRIVATE rows too, and the open's private records (linker.f OWNED-TEXTS, MAKE-OWNED) carry facts rendered from them, with no row stated twice; HB-OWNED and OWN-OR-ROWS's record arm are deleted. Files: src/habu/linker.f, src/core/check-hook.f, src/arch/arm64/link-kernel.f. Verify: ~/.cache/tmp/jerry-strip/of1.f is on no build path; EPPRIM: CHECKER-OVERLAY imm-mark builds; engine fixpoint; consumer measure. Depends: 2c. Ownership: the lane. Claim: unassigned (brief to write).
