---
title: Move the arch-suffixed engine files into src/arch/<isa>/
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T16:31:28.945118+02:00"
---

Problem: Joel 2026-11-05 (DECISIONS.md "where machine-specific code lives") ruled one directory per instruction set under src/arch/<isa>/, while the arm64 and x86-64 engines grew in src/habu as suffixed files. Acceptance: arm64 now: src/habu/t0-arm64.f and src/habu/runtime-arm64.f move to src/arch/arm64/ as a plain move (code as is, names unchanged; only require paths and by-name strings follow); x86-64 at step 8 (x86 gets no work until arm64-to-arm64 works): boot-x64.f, kernel-x64.f, link-x64.f, t0-x64.f, runtime-x64.f, image-x64.f, patch-x64.f, prof-x64.f, snap-x64.f, snap-decode-x64.f, text-sites-x64.f, kernel-hir-x64.f, aot-x64.f, aot-x64-format.f to src/arch/x86-64/. Files step 7 deletes stay (habu1.f, habu2.f, jit.f, rt.f, regalloc.f, aot-arm.f, aot-link-arm.f). Files: arm64 readers src/habu/layout.f, src/habu/native-runtime.f, src/habu/runtime-arm64.f, src/habu/t0-emit.f, src/habu/t0-label.f, tools/manifest-lint-core.f, plus hb-build's startup list and any test naming the paths (search at brief time). Verify: rebuild bin/hb (e1, e2 maps same), the suite's compiler and T0 tests through the moved files. Depends: none for arm64; x86-64 part waits for step 8. Ownership: the moved files and their readers. Claim: unassigned.

Note 2026-11-10 (bob): src/habu/aot-x64.f and src/habu/patch-x64.f are deleted (habu-nothing-loads-src-66ea20d9); nothing loaded them.
