---
title: "Write hb's require registry as data: REQUIRE-REG:BOOT-OPEN and BOOT-FREEZE go"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T08:09:25.737251+02:00"
---

Problem: tools/hb-build-core.f END-WRITE (:258-266) fills the new hb's require registry by writing text the new hb's own include.f runs: REQUIRE-REG:BOOT-OPEN, one row per file, REQUIRE-REG:BOOT-FREEZE (src/core/include.f). Both are public words that flip a flag read elsewhere (REQUIRE-BOOT-V, REQUIRE-BOOT-N; REQUIRE-BOOT-LIMIT :94), and any program can say REQUIRE-REG:BOOT-FREEZE and mark its own files as the engine's in the hb doing its build. test/source-root-exe-test.f:534 (BOOT-OUTSIDE) has a program say REQUIRE-REG:BOOT-OPEN. The staged globals REQUIRE-BOOT-OPEN and REQUIRE-BOOT-FREEZE go by habu-delete-the-require-835ee1dc. Joel 2026-11-11: 'why do you need REQUIRE-BOOT-FREEZE? are you fixing the symptoms or the cause?'. Not established: whether the build can store the rows and the count into the product's data with no word run in the product. Acceptance: the trace answers it; then the registry's engine rows and count are data the build writes and REQUIRE-REG:BOOT-OPEN, BOOT-FREEZE and REQUIRE-BOOT-V are deleted. Depends: none. Claim: unassigned.
