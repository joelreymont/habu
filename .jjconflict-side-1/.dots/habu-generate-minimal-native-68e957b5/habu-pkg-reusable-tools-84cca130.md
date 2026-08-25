---
title: Package reusable tools
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T21:30:25.192716+02:00"
---

A frozen f7ed6085 census finds another 1,054 unowned globals in reusable tool/core pairs outside the shared lint and checker closures: build-fixpoint.f 322; cli-run.f 25; gate-json-assert-core.f 115; stdin-closure-lib.f 14; ddc-verify.f 46; typed-local-diff-lint-core.f 66; repair-packet-core.f 42; kernel-perf-lint-core.f 36; jitdump-core.f 9; diag-to-sarif-core.f 99; aot-call-report-lib.f 101; seed.f 48; imgdump.f 96; imagedisasm.f 32; checker-assert.f 1; why-threw.f 2. Concrete generic collisions include NOP-INSTR, BL-MASK, WORD-BYTES, REPORT-PATH-CAP, JSON-NUM-CAP, W32@, and JD. Give each concern its named package (BUILD-FIXPOINT, CLI-RUN, JSON-ASSERT, STDIN-CLOSURE, DDC, TYPED-LOCAL-LINT, REPAIR-PACKET, KERNEL-PERF-LINT, JIT-DUMP, DIAG-SARIF, AOT-CALL-REPORT, SEED, IMAGE-DUMP, IMAGE-DISASM, CHECK-ASSERT, WHY-THREW); export only MAIN and actual cross-file library entry points, tests reopen their owner, and remove legacy globals. This controller must split by owner before dispatch and coordinate existing diff-parser work without duplicating behavior. Preserve every CLI byte/exit, focused test, build/fixpoint result, dump/disassembly output, repair schema, and lint finding. Acceptance: standalone load/CLI parity, old/private reject, qualified public certify, dictionary-name/JIT/DATA/CODELEN shrink, no performance regression, focused suites plus package/host/dot/fixpoint/full native gates green. Excludes HB-BUILD and clobber-lint migrations already owned.
