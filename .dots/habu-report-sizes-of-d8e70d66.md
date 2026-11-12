---
title: Report sizes of what the linker writes
status: active
priority: 3
issue-type: task
created-at: "2026-11-09T18:37:14.518818+02:00"
---

Problem: tools/image-size-lib.f reads only the old builders' layouts. Since hb-build builds through the linker, --size-report and --report-json fail on every hb product after -o is written: a warm `hb-build --engine --report-json -o X` exits 74 with 'image-size: no AOT payload in the image' (image-size-lib.f FIND-AOT) and X, X.map already replaced; `bin/hb --load tools/engine-size.f -- bin/hb` dies the same way. On a stripped program the report succeeds but its numbers (total 51352, code 7344, names 0, data_written 0, other 19296) come from the deleted LINK-ARM layout. tools/hb-build.f's usage line offers both options and tools/AGENTS.md names engine-size.f; no test passes either option. image-size-lib.f CODE-BLOB-RANGE's comment names native-build's .map sidecar. Found by the step-5 build-path review (2026-11-09). Acceptance: the size report reads the program and .map the linker writes (src/habu/linker.f, src/habu/link-map.f) for --engine, --repl and stripped builds; engine-size.f reads bin/hb; one suite case passes --size-report and --report-json on a build it already makes; the old-layout readers go. Files: tools/image-size-lib.f, tools/engine-size.f, tools/hb-build-lib.f, tools/hb-build-report.f, the case's test file. Verify: the case passes; engine-size.f on bin/hb prints sizes summing to its file size. Depends: none. Ownership: those files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-size.

Moved from habu-delete-the-dead-c212d5a2: the old-layout readers that go include tools/aot-startup-shape.f and image-size-lib.f's stripped-image section, which parse the deleted LINK-ARM output; their comments describe a sparse DATA blob, a presence-map copy loop and x11/x12 registers that src/arch/arm64/boot.f START does not write (image-size-lib.f ~2436-2451, 2559, 2581, 2607, 2637-2640, 2686-2688; aot-startup-shape.f 29-30, 59-65, 79-87). src/arch/arm64/asm.f also names aot-startup-shape.f.

Acceptance case (jerry): a --target wasm build with --size-report ends `image-size: not a supported Habu ELF executable`, rc 74; the report reads the module and its .map as it reads an arm64 program.
