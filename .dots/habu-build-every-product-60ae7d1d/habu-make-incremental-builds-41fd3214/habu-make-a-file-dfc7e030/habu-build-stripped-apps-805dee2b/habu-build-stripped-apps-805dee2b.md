---
title: "Build stripped apps through LINKER:WITH in-process"
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T12:06:26.045948+02:00"
blocks:
  - habu-s5c-load-hb-6aae35fd
---

Step 5a of the cross-build order (/tmp/jerry-big/xbuild-order.md; DECISIONS tools in hb, linker's build word). Problem: hb-build's stripped mode runs a maker child on a keyed builder image. Acceptance: hb-build's stripped mode runs LINKER:WITH in its own process, production and suite; tools/aot-builder-image.f, HBB-MAKER-IMAGE$ and the maker child in tools/hb-build-lib.f, test/aot-builder-engine.f and its gate-images family (test/gate-images.f:37,45) go; test/linker-builder-engine.f and test/preloaded-engine.f are already gone at 714e5bd0. Acceptance case from the step-4a review: a stripped arm64 app calling ptr-cell! links (today the old linker refuses with rc 74: BL into (MARK) mid-record, aot-closure.f FINDADDR-PTR; probe ~/.cache/tmp/rev-ptrcell/probe/pcs.f). Files: tools/hb-build*.f, tools/aot-builder*.f, test/gate-images.f and the named tests. Verify: the stripped-app tests and the probe. Depends: step 5 (LINKER:WITH opens the space) and habu-s5c-load-hb-6aae35fd (jerry: hb's startup files load into a LINKER:WITH space). Ownership: those files. Claim: unassigned.

Census (carl, 2026-11-06, dot habu-census-hb-build-371c0805; reports .jj-ws.noindex/carl-dots/.out/carl-file/s7-census-apps.md, s7-features.md, s7-census-q4.md): of the 21 apps the stripped suites build, only stripped-pool-reach links and runs through LINKER:WITH + LINKER:WRITE today. 20 stop at E-UNDEFINED on their first hb word (`require`, PERSISTED-PTR-VARIABLE, TMP-PATH-COPY-SRC): a build's space sees only its own words and WL:PRIMITIVES (src/habu/linker.f:132-134, habu1.f:6082). Design r2 3.4/3.8 has every app build load hb's startup files into the space first. jerry's S5a-2 census of that load stopped at src/core/util.f:7 (the checker's duplicate refusal, which S5b2-1 fixes); the later startup files were not reached. So this dot depends on the startup files loading into a LINKER:WITH space. The moves that go with the maker child: engine selection (HBB-SELECT-ENGINE tools/hb-build-lib.f:969), the maker timeout (:416-424, :687-699), the producer key and support record (:527-635), and the counts and inputs-manifest files (:443-470, :663, :923). The missing-MAIN refusal (aot-closure.f:925, test tools/hb-build-stripped-test.f:181-194) moves into the driver. Baseline for the eight stripped suites: about 35 s on the critical path (builder image 13.1 s, then hb-build-stripped 22.0 s).

Acceptance from S6 L5's docs (carl, 2026-11-06): docs/architecture.md "Images", second paragraph ("The window holds only what the application requires"), describes the maker child's capture window; it goes with the maker child, or states the rule for a build's own space. docs/compilation.md "Building an image"'s Now line then drops its clause that only test/compiler/arm64-link-run.f calls LINKER:WRITE.
