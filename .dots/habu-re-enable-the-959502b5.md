---
title: Re-enable the x86-64 tests
status: open
priority: 2
issue-type: task
created-at: "2026-11-09T19:14:21.819299+02:00"
blocks:
  - habu-emit-wasm-from-0d5dab47
---

Joel 2026-11-09: "disable lsp and x86 and wasm. get arm-to-arm done first, including pg. get that fully working then tackle wasm and x86. do not delete tests unless they are absolutely not load-bearing, disable them. add ifdefs if we don't have those". Third step, after Wasm works (habu-emit-wasm-from-0d5dab47). The 21 x86-64 suite tests that stop at load (rc 70) on the landing tree are disabled with `[if]` in the landing stack until this dot re-enables them.

Problem (jerry, 2026-11-09): three arm64 changes never reached the x86-64 twins. (1) ozrpuyuu put src/os/image-bytes.f in package IMAGE-BYTES and src/habu/driver-io.f in DRIVER; src/os/linux-x86-64/elf.f:101, test/x86-64-seam.f:35 and test/x86-64-peer-harness.f:305,308 name their words bare (E-UNDEFINED MSIZE, MBUF, DRV-WRITE-IMAGE, M-OFF). (2) rorqpwpw: a target writer names its buffer size with MSIZE! and requests image-bytes.f before its scopes (src/os/linux/elf.f:79); the x86-64 writer still runs ELF-MSIZE-CHECK. (3) ymlrwntu deleted arm64's seed-ndict!; src/habu/kernel-x64.f still defines it, so the primitive-registry check exits 76 ('primitive with no row in src/habu/prims.f: seed-ndict!'). Also: hb now carries image-bytes.f, macho.f and driver-io.f, so the x86-64 tests' `require` of them binds to hb's copies; DRV-WRITE-IMAGE, the one driver word they use, is target-neutral.

Parked commit plrptunw ('Follow arm64's writer changes into x86-64', a sibling of the landing stack) fixes all three: with it the 28 x86-64 suite tests pass on the arm64 host (they build images and check bytes; no x86-64 machine ran them). It deletes kernel-x64.f SEED-NDICT-BODY, prof-x64.f's REWIND helper, test/x86-64-kernel-engine.f's BUILD-SEED-NDICT cases and test/x86-64-kernel-prof.f's REWIND-CASE and RETIRED-CASE. RETIRED-CASE's premise was the seed rewind, but Joel allows deleting only tests that are absolutely not load-bearing: keep it on ndict! instead unless an x86-64 run shows it duplicates TRIM-CASE.

Acceptance: plrptunw rebased onto master, the x86-64 tests re-enabled and passing. Files: those named. Verify: bin/hb --load test/run.f -- with every x86-64 suite name. Depends: habu-emit-wasm-from-0d5dab47. Ownership: those files. Claim: unassigned.

Case (jerry, 2026-11-10), from the process-state design (habu-design-no-file-298feb05): src/habu/kernel-x64.f's `scratch-cell-mark` converts a declared pointer cell, as src/habu/habu2.f's does once that design's mark change lands. The twin changes by reading in the commit that changes habu2.f; this dot checks it on an x86-64 build: a declared pointer cell marked scratch is left out of a file object and links zero.

Case (kevin, 2026-11-10), from the loader record (habu-keep-active-src-2da0b7b5): src/core/include.f stores its record's address in SRCLOC:SOURCE-CELL ($2C48) with ptr-cell! at top level, a DATA pointer in a fixed header cell that tools/native-layout.f SLOTS does not list, so by reading NATIVE-LAYOUT:TRANSLATE-ROWS refuses it ("incompatible fixed engine layout"). No row was added while the x86-64 route is off (test/x86-64-link-records.f stops at elf.f:101 MSIZE on that tree too): this dot adds `$2C48 , 1 ,` beside TASK-CHAIN-CELL and checks it on an x86-64 build.
