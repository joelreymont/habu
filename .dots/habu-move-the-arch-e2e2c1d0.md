---
title: Move the arch-suffixed engine files into src/arch/<isa>/
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T16:31:28.945118+02:00"
---

Problem: Joel 2026-11-05 ruled one directory per instruction set under src/arch/<isa>/, while the arm64 and x86-64 engines grew in src/habu as suffixed files. Acceptance: arm64 now: src/habu/t0-arm64.f moves to src/arch/arm64/ as a plain move (code as is, names unchanged; only require paths and by-name strings follow); x86-64 at step 8 (x86 gets no work until arm64-to-arm64 works): boot-x64.f, kernel-x64.f, t0-x64.f, prof-x64.f to src/arch/x86-64/. Files step 7 deletes stay (habu1.f, habu2.f, rt.f). Files: arm64 readers src/habu/layout.f, src/habu/t0-emit.f, src/habu/t0-label.f, plus hb-build's startup list and any test naming the paths (search at brief time). Verify: rebuild bin/hb (e1, e2 maps same), the suite's compiler and T0 tests through the moved files. Depends: none for arm64; x86-64 part waits for step 8. Ownership: the moved files and their readers. Claim: unassigned.
