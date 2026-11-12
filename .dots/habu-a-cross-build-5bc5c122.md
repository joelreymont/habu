---
title: A cross-build refuses the code address a callback row holds
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T09:05:34.279481+02:00"
---

Problem: a program with one CALLBACK: fails --target linux-aarch64 on macOS: 'linker: the cell at space +4239328 in EMIT +928 holds CMP-ENTRY +12, which an object cannot name', E-COMP-STATE (-8570), src/habu/linker.f REFUSED; it builds natively. Cause: the target copy of a callback accessor has no C entry. src/habu/t0.f LOWER runs SHADOWED before HOSTED and arms the stub only in HOSTED (STUB-ARM); tier 1 has the same order (src/compiler/native/compiler.f SHADOWED, STUB-ARM). So for the row's entry cell (src/lib/ffi-callback.f INSTALL-ENTRY) SOURCE-FUNCTION answers -1 -1 and linker.f CELL-REF+ refuses. Acceptance: the program of test/stripped-callback-subject.f builds for linux-aarch64 and sorts on Linux arm64 as natively. Depends: none. Claim: agent=jerry (the T0 and tier-1 stub edit)
