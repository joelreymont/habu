---
title: Name the exit in the x86 build window
status: closed
priority: 2
issue-type: task
created-at: "2026-10-01T09:04:08.241514+03:00"
closed-at: "2026-10-01T09:51:28.872643+03:00"
close-reason: "Post-seal source compiles through the window's installed compiler; WRITER-MACHINE-CK stops the x86 window before the writer (rc 74, named fd 2 line, was 67/83); native-build-entry ok; host rebuild sha e11cab55 unchanged"
---

Problem: on spark, `bin/hb --load src/arch/x86-64/backend.f tools/native-build.f -- <out> --target linux-x86-64` (S lane, `habu-select-the-build-610b4492`, engine 2ef07f9f) loads the x86 seam files, runs the capture, then exits 83 (SEAL-VIOLATION) with nothing on fd 2, after opening `tools/native-emit.f` and then `src/arch/arm64/icode.f`. The S worker inferred, unverified: in a host window the ARM64 backend loads icode.f before the capture freezes; the x86 window has no ARM64 backend, so icode.f loads for the first time after the seal.
Acceptance: the cause is identified and fixed at its layer; until the x86 writer exists (X2b/X4) the window stops before the writer with a named refusal on fd 2, never a silent 83; a host-target build is unchanged (chain).
Files: `tools/native-build-core.f`, `tools/native-emit.f`, `tools/native-build.f` as the cause dictates; `test/native-build-entry.f`.
Verify: the command above prints the refusal and its rc; `test/native-build-entry.f`; rebuild byte-identical.
Ownership: krait (Intel lane).
