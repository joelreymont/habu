---
title: "Write hb's require registry as data: REQUIRE-BOOT-OPEN and REQUIRE-BOOT-FREEZE go"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T08:09:25.737251+02:00"
---

Problem (2026-11-11, by reading): tools/hb-build-core.f END-WRITE (:225-234) fills the new hb's require registry by writing text the new hb's own include.f runs: REQUIRE-BOOT-OPEN, one row per file, REQUIRE-BOOT-FREEZE (src/core/include.f:66 and :2365; the arm64 boot prefix emits the OPEN token, src/habu/habu2.f:1464). Both are public words that flip a flag read elsewhere (REQUIRE-BOOT-V, REQUIRE-BOOT-N; REQUIRE-BOOT-LIMIT :76, BOOT-ROW :1343), and any program can say REQUIRE-BOOT-FREEZE and mark its own files as the engine's in the hb doing its build. Joel 2026-11-11: 'why do you need REQUIRE-BOOT-FREEZE? are you fixing the symptoms or the cause?'. Not established: whether the build can store the rows and the count into the product's data with no word run in the product (DECISIONS 'how Habu builds Habu'), and whether habu2.f's prefix token is still on a path a build takes. Acceptance: the trace answers both; then the registry's engine rows and count are data the build writes, the two words and REQUIRE-BOOT-V are deleted, and tools/bundle-lib-core.f still bundles no file the engine loaded. Files: tools/hb-build-core.f, src/core/include.f, src/habu/habu2.f. Verify: engine build, chain check, test of bundle-lib; suite once. Depends: none. Ownership: unassigned. Claim: unassigned.
