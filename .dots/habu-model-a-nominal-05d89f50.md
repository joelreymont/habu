---
title: Model a nominal ENUM fetch from typed storage in ncomp
status: open
priority: 3
issue-type: task
created-at: "2026-09-17T18:59:01.689394+03:00"
---

Problem: inside tools/native-build.f's window the native compiler refuses 'BACKEND-ROWS TYPED-BUFFER B-ARCH CTARGET:arch' followed by 'i B-ARCH @ drop' with 'ncomp: cannot compile FIND-ROW' / E-HIR-UNMODELED (-8286), while the same code compiles and runs in a booted engine and under EXECUTABLE-BUILD:WITH (registry lane, 2026-09-17, reduced by bisection); the registry in src/compiler/target.f keys its rows by ARCH-CODE (a plain n) instead of the arch value because of it, and the gap silently pushes typed storage back to plain cells wherever the window compiles. Acceptance: the HIR dialect models the fetch of an arity-0 ENUM value from a TYPED-BUFFER (and TYPED-VARIABLE) in the native-build window exactly as the booted engine does, with a regression under test/compiler/ that compiles such a word in the window; the registry's key may then return to the arch value in a follow-up. Files: src/compiler/native/ (the HIR lowering of typed-storage fetches), src/core/ typed storage if the row shape is the cause, test/compiler/. Verify: the regression; tools/native-build.f from the tree; test/run.f. Depends: none. Ownership: native compiler. Claim: unassigned.
