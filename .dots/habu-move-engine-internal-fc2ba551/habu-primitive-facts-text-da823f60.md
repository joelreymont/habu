---
title: "Primitive facts text comes from the kernel object's rows"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-11T10:34:54.953226+02:00"
closed-at: "2026-11-11T13:57:58.890695+02:00"
close-reason: landed as tpxorvpp; a build that states a kernel object lays owned records from its K-PKG-PRIVATE rows; engine fixpoint hb1 = hb2, suite and chain rc 0, consumer measure m15 green
---

Problem: privacy step 2d (privacy-design.md Design 1): src/habu/linker.f HB-OWNED finds an owned primitive's (prims.f EPPRIM:, kind K-PKG-PRIVATE) facts text by name in the host's private wordlist, which a stripped host lacks, and src/core/check-hook.f OWN-OR-ROWS returns that host record's text first. Step 2c (dot habu-a-build-s-87c9c7c2) takes the global primitives: PRIM-TEXT and PRIMITIVE-FACTS render from the rows the build stated, and its replay (link-kernel.f STATE-PRIMS) skips K-PKG-PRIVATE rows. Acceptance: the replay states the K-PKG-PRIVATE rows too, and the open's private records (linker.f OWNED-TEXTS, MAKE-OWNED) carry facts rendered from them, with no row stated twice; HB-OWNED and OWN-OR-ROWS's record arm are deleted. Files: src/habu/linker.f, src/core/check-hook.f, src/arch/arm64/link-kernel.f. Verify: the probe of CHECK:OWNED-FACTS for an owned private symbol is on no build path; EPPRIM: CHECKER-OVERLAY imm-mark builds; engine fixpoint; consumer measure. Depends: 2c. Ownership: the lane. Claim: agent=jerry workspace=.jj-ws.noindex/priv-2d (brief .jj-ws.noindex/jerry-wasm/.out/briefs/priv-2d.md).

Scope at dispatch 2026-11-11 (jerry): a build that states a kernel object lays its owned records from the object's K-PKG-PRIVATE rows (ARM64:OWNED-N, OWNED-PKG$, OWNED-NAME$), texts rendered from the rows STATE-PRIMS states. A build that states no object (a kernel build, a Wasm program) keeps hb's PRIM-SPEC rows with HB-OWNED's texts: the installed hb's checker has no rows for an owned private symbol (CHECK:OWNED-FACTS with no record answers empty), so HB-OWNED and OWN-OR-ROWS's record arm go once Wasm writes a kernel object. The lane's probe is the OUTER-to-INTERPRETER rename (dot habu-merge-the-interpreter-8de1d38d), which a pre-2d host cannot build.
