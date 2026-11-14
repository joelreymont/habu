---
title: "Write hb's require registry as data: REQUIRE-BOOT-OPEN and REQUIRE-BOOT-FREEZE go"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T08:09:25.737251+02:00"
---

Problem: tools/hb-build-core.f END-WRITE (:217-225) fills the new hb's require registry by writing text the new hb's own include.f runs: REQUIRE-BOOT-OPEN, one row per file, REQUIRE-BOOT-FREEZE (src/core/include.f:63 and :2223). Both are public words that flip a flag read elsewhere (REQUIRE-BOOT-V, REQUIRE-BOOT-N; REQUIRE-BOOT-LIMIT :71, BOOT-ROW :1238), and any program can say REQUIRE-BOOT-FREEZE and mark its own files as the engine's in the hb doing its build. test/source-root-exe-test.f:525 (BOOT-OUTSIDE) has a program say REQUIRE-BOOT-OPEN. Joel 2026-11-11: 'why do you need REQUIRE-BOOT-FREEZE? are you fixing the symptoms or the cause?'. Not established: whether the build can store the rows and the count into the product's data with no word run in the product. Acceptance: the trace answers it; then the registry's engine rows and count are data the build writes, the two words and REQUIRE-BOOT-V are deleted, and tools/bundle-lib-core.f still bundles no file the engine loaded. Depends: none. Claim: unassigned.
