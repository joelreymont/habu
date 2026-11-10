---
title: "Give x86-64's writer files the arm64 packages"
status: open
priority: 2
issue-type: task
created-at: "2026-11-09T12:46:06.819189+02:00"
---

Problem: carl's packaging of the kernel writer (DECISIONS 'the kernel writer's names') moved the arm64 and shared writer files into packages ENGINE-EMIT, CRASH, SYSCALL, IMAGE-BYTES, EXECUTABLE, REGALLOC, RT, DRIVER; src/habu/driver-io.f now opens 'using EXECUTABLE IMAGE-BYTES'. The x86-64 side was left untouched because x86-64 gets no work until arm64 lands (DECISIONS 'the fastest path to arm64'), so x86-64 builds that load the shared files now miss names. Acceptance: src/arch/linux-x86-64 elf.f and sign.f join EXECUTABLE like linux arm64's elf.f/sign.f; image-x64.f, aot-x64.f, link-x64.f, test x86-64-peer-harness.f, x86-64-seam.f and test/wasm/link.f add 'using IMAGE-BYTES' (and EXECUTABLE where they name its words); the x86-64 sys/rt/proc twins get SYSCALL/RT packages like arm64's; src/arch/linux-x86-64/elf.f moves its requires (image-bytes.f among them) to the file top, before any 'using', and calls IMAGE-BYTES:MSIZE! with its exact maximum instead of checking the old MSIZE constant, as arm64's macho.f and elf.f now do (image-bytes.f's MSIZE is a word and MSIZE! sets the buffer size). Proof: the x86-64 build on pulsar and the suite's x86-64 tests pass. Files: as listed. Verify: x86-64 build + suite x86-64 tests on pulsar. Depends: arm64 landing (Goals 'arm64'). Ownership: those files. Claim: unassigned.

Note 2026-11-10 (bob): src/habu/aot-x64.f and src/habu/patch-x64.f are deleted (habu-nothing-loads-src-66ea20d9); nothing loaded them.
