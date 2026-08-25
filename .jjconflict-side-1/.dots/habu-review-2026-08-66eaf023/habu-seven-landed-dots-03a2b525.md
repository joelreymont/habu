---
title: seven landed dots still open and ready
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:26.034299+02:00"
---

Problem: open leaves whose contract the tree already satisfies (package opener present, 0 definitions outside): habu-pkg-profiler-emitter-441f0508 (src/habu/prof.f:14 package PROF, commit efc730a6), habu-pkg-aot-linker-3e1ae3e2 (aot-lib.f:23 AOT-LINK), habu-pkg-aot-capture-3b70e1c9 (aot-capture.f:21), habu-pkg-snapshot-writer-bbee5c91 (snap-lib.f:20 SNAP), habu-pkg-public-signatures-e25db8b1 (public-signatures-core.f:11 PS), habu-pkg-err-code-9508f911 (error-code-lint-core.f:52), habu-pkg-checker-tools-fe04934e (check-core.f:30 CHECK); partials: habu-pkg-ptx-autodiff-d15a611e (ad.f done; ad-dag.f 60, ad-gen.f 45, ad-ir.f 15 outside), habu-pkg-the-arm64-ffabc063 (asm.f done; icode.f 103, mnem.f 60 outside). Acceptance: the seven closed citing the landing commit after checking each leaf's secondary acceptance; the two partials re-scoped to the remaining files. Files: the nine leaves. Verify: dot ready no longer lists them. Depends: none. Ownership: tracker. Claim: unassigned.
