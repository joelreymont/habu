---
title: Build AOT test rows on the image
status: closed
priority: 3
issue-type: task
created-at: "2026-10-02T09:56:33.903503+02:00"
closed-at: "2026-10-14T19:30:56.978744+03:00"
close-reason: "Implemented in 9161fa93: AOT test rows use image builds when their actual closure permits; remaining engine-built rows have observed pre-window refusals and comments. Focused suites and interleaved wall-time measurements recorded."
---

Problem (r4-spanrow lane, 6000470d, dot 8ac655f6): the span row built on the engine for a stale reason and moving it to the linker image saved ~18 s per hb-build-aot-test run. Other rows still build on the engine with the -SOURCE prepare words: tools/hb-build-aot-test.f:280 BUILD-AOT-FFI (src/lib/ffi-abi.f, not baked; no comment says why) and rows in the stripped, lifecycle, chain and large-source build tests. The rule (tools/hb-build-test-lib.f HBT-KEYED! comment, docs/gate.md) is: build on the engine only a program reaching a module of the linker's lib closure the engine does not bake. Acceptance: census every -SOURCE prepare call in tools/ and test/; each row either moves to the image (rc 0, before/after wall time on interleaved runs) or keeps the engine with a comment naming the unbaked closure module, proven by building it on the image and showing the E-AOT-PRE-WINDOW refusal; the rows' tests rc 0. Base: after the master merge (master changed tools/hb-build-stripped-quotation-field-test.f).
