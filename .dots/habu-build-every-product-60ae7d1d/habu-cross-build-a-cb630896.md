---
title: Cross-build a linux-aarch64 engine from macOS
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T10:56:42.187456+02:00"
---

Problem: native-build --target linux-aarch64 on macOS stops with 'not executable here' (timer lane 1, 2026-10-30), so a Linux engine cannot be produced on the Mac; the roadmap's finish line says a cross-built engine starts. Acceptance: the cross build writes a Linux arm64 engine on macOS and that engine boots on spark. Files: tools/native-build-core.f, tools/native-build.f. Verify: the cross-built engine on spark runs bin/hb --load test/run.f. Depends: none. Ownership: tools/native-build*.f. Claim: unassigned.

Needed by: the checker-cost timings on spark (habu-cut-the-checker-6c4079af, 2026-11-06 note): no hb on spark builds today's tree, so a Linux arm64 engine has to come from the Mac. The old capture builder's `--target` path is machinery being replaced (CLAUDE.md); the engine comes from the linker's cross-build with a Linux file writer.

Acceptance case (stuart, 2026-11-07): a Linux arm64 hb cross-built on the Mac by the old capture builder (~/.local/lib/habu, `--target aarch64-unknown-linux-gnu`, the same source as spark's tree) boots on spark, runs Tender's corpus (5 forms; libzip.so.5 from ~/stuart-runs/lib) and builds itself with rc=0, but the product is not byte-equal: spark:~/stuart-runs/habu/bin/hb vs ~/stuart-runs/hbo/hb2, both 7,143,616 bytes, 2,644,568 bytes differ from byte 6885 (0xd3 vs 0x0b), while the two hb.map files are identical (build log ~/stuart-runs/hbo/b2.log). Not yet diagnosed. With the engine written by the linker's cross-build, a Linux arm64 hb cross-built on the Mac equals the hb that hb builds of the same tree on spark.
