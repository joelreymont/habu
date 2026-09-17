---
title: Preserve debugger watchpoint resume in native products
status: active
priority: 1
issue-type: task
created-at: "2026-09-13T16:50:35.271296+03:00"
---

Fresh 316-suite integration run on native engine e27fbfac9948 fails engine-runtime-regressions at process/pty. The captured PTY reaches watchpoint output for 2 WID . and then SIGSEGV at PC 0x417ff0; proc-pty assertion F144 is the first failure, later failures follow the dead child. This is distinct from historical F10 pool-contention reports and is not yet attributed to the provider patch. Own src/habu debugger/watchpoint signal-resume path and test/proc-pty.f only as reduction proves; preserve watchpoint printing and resume semantics. Cedar owns diagnosis. Compare original28, unchanged-source native B1, and e27 products with the same focused PTY test, reduce the first failing command, inspect the native crash using docs/debugging.md, fix the responsible layer, add a targeted regression. Acceptance: debugger watchpoints, persistent/one-shot/skip breakpoints and subsequent REPL commands continue correctly in first and repeated native products; focused engine-runtime test and full native gate pass. Evidence: .jj-ws/cedar-correctness-verify/build/full-gate-provider.log and /tmp/habu-native-suite-1040769606128000-17/pool-1253830-266-out.log. Do not increase timeouts or infer a PTY flake from later assertions.

Claim: Cedar diagnosis/integration; compare the first failing debugger command across identified native products before modifying its responsible layer.

Confirmed mechanism: one-shot restore used PROT:LOPEN at live CP, but the saved breakpoint instruction was on an earlier 64 KiB protection page. The SIGSEGV is the subsequent STRW to that read-only instruction, not a watch-table or PTY timing defect. C-BP-RESTORE-ONESHOT now declares the actual four-byte target span through PROT:LSPAN. The recovery emitter still protects the entire region and does not share this mistaken CP span.

Verification: test/debugger-resume.f forces distinct target/cursor pages, prints the watched value, resumes the target and executes it again. It fails with SIGSEGV134 on e27fbfac9948 and passes on freshly rebuilt7e0ef328a6fe. The real runtime-regression-test also passes its process/PTY section, covering one-shot/persistent/skip breakpoints and REPL recovery; final suite completion is recorded with integration. Full combined native gate remains pending.
