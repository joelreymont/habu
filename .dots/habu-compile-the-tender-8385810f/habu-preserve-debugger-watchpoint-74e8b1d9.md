---
title: Preserve debugger watchpoint resume in native products
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-13T16:50:35.271296+03:00\""
---

Fresh 316-suite integration run on native engine e27fbfac9948 fails engine-runtime-regressions at process/pty. The captured PTY reaches watchpoint output for 2 WID . and then SIGSEGV at PC 0x417ff0; proc-pty assertion F144 is the first failure, later failures follow the dead child. This is distinct from historical F10 pool-contention reports and is not yet attributed to the provider patch. Own src/habu debugger/watchpoint signal-resume path and test/proc-pty.f only as reduction proves; preserve watchpoint printing and resume semantics. Cedar owns diagnosis. Compare original28, unchanged-source native B1, and e27 products with the same focused PTY test, reduce the first failing command, inspect the native crash using docs/debugging.md, fix the responsible layer, add a targeted regression. Acceptance: debugger watchpoints, persistent/one-shot/skip breakpoints and subsequent REPL commands continue correctly in first and repeated native products; focused engine-runtime test and full native gate pass. Evidence: .jj-ws/cedar-correctness-verify/build/full-gate-provider.log and /tmp/habu-native-suite-1040769606128000-17/pool-1253830-266-out.log. Do not increase timeouts or infer a PTY flake from later assertions.

Claim: Cedar diagnosis/integration; compare the first failing debugger command across identified native products before modifying its responsible layer.
