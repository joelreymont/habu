---
title: "A build's checker takes its primitive rows from the kernel object"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T10:34:54.939455+02:00"
---

Problem: privacy step 2c (privacy-design.md Design 1): a build's checker starts from the host hb's primitive rows (src/core/checker.f CHECK:OPEN's prefix; PE-SPEC-NEW states only primitives the host lacks), so a prims.f effect edit reaches no built engine. Measured 2026-11-11 on the quotation line: the READ-ROW change that keeps a primitive row's quotation inputs reaches SOURCE-ROOT:RUN-CLOSED's own facts only in the second engine generation (~/.cache/tmp/quot-astra/p/run-closed.f: rc 139 on the first, rc 70 on the second). Acceptance: before the first file the linker states the kernel object's rows (link-kernel.f CHECK-ROWS-AT) to the checker by the atom replay PE-SPEC-ROW does; CHECK:OPEN's prefix holds the grammar rows alone; PE-SPEC-NEW is deleted; the object's rows section is required. Files: src/core/checker.f, src/habu/linker.f, src/arch/arm64/link-kernel.f. Verify: a changed effect in prims.f shows in a wrapper's facts after one build; engine fixpoint; consumer measure. Depends: 2b. Ownership: the lane. Claim: unassigned (brief to write).
