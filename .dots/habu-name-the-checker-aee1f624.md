---
title: "Name the checker owner's offsets once"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T23:15:28.351817+02:00"
---

Problem: src/habu/layout.f:620-684 defines 56 constants NCOMP-DISPATCH:DECL-<X>-OFF, each equal to CHECKER-OWNER-ABI:<X>-OFF (src/core/checker-owner-abi.f), so every owner-record offset has two names and callers mix them: src/habu/verify-source.f names most dispatch offsets through the aliases (RECORD-SYM? :1322) but CSR-ARM (:1294) and the trust and binding rows (:2797, :2829) name CHECKER-OWNER-ABI:* directly, in checked code. The comment that gave the aliases' reason (a source boot has no checked row for CHECKER-OWNER-ABI's constants) was false and is deleted in the comment lanes; those direct uses build and check. Readers of the aliases (rg -i --no-ignore over src tools test): src/compiler/native/checker-owner.f 19, src/habu/interpreter.f 23, src/habu/verify-source.f 22, src/habu/habu2.f 9, src/habu/kernel-x64.f 8, test/checker-decl-locs-lib.f 4, test/x86-64-kernel-engine.f 2, test/replay-scope.f 2, src/habu/habu1.f 2; every alias has a reader. Acceptance: one name per offset: every reader names CHECKER-OWNER-ABI:<X>-OFF and the 56 alias lines in layout.f are gone, NCOMP-DISPATCH:DECL-A and the cells beside them unchanged. Files: src/habu/layout.f and the readers above. Verify: rg finds no DECL-<X>-OFF alias name; engine built twice byte-equal, chain fixpoint, full suite, census. Depends: the x86-64 step for kernel-x64.f and test/x86-64-kernel-engine.f (x86-64 is set aside: no x86 build until its step). Ownership: the files above. Claim: unassigned.
