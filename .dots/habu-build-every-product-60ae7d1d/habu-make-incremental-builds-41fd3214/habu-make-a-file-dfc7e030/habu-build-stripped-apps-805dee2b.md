---
title: "Build stripped apps through LINKER:WITH in-process"
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T12:06:26.045948+02:00"
---

Step 5a of the cross-build order (/tmp/jerry-big/xbuild-order.md; DECISIONS tools in hb, linker's build word). Problem: hb-build's stripped mode runs a maker child on a keyed builder image. Acceptance: hb-build's stripped mode runs LINKER:WITH in its own process, production and suite; tools/aot-builder-image.f, HBB-MAKER-IMAGE$ and the maker child in tools/hb-build-lib.f, test/aot-builder-engine.f, test/linker-builder-engine.f, test/preloaded-engine.f and their gate-images families go. Acceptance case from the step-4a review: a stripped arm64 app calling ptr-cell! links (today the old linker refuses with rc 74: BL into (MARK) mid-record, aot-closure.f FINDADDR-PTR; probe ~/.cache/tmp/rev-ptrcell/probe/pcs.f). Files: tools/hb-build*.f, tools/aot-builder*.f, test/gate-images.f and the named tests. Verify: the stripped-app tests and the probe. Depends: step 5 (LINKER:WITH opens the space). Ownership: those files. Claim: unassigned.
