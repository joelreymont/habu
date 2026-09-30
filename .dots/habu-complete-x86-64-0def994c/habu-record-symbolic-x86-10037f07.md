---
title: Record x86 call and code sites with exact spans
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.475867+03:00"
---

Problem: `wordcall`/`tailcall` resolve to absolute host entries at emit time (`docs/x86-64.md:192-206`) and `TRAILING-RETURN?` mirrors ARM64's size-minus-one rule; the publisher, the capture and the linker need every site recorded and every span exact.
Acceptance: every inter-word call, tail call, `codeaddr` and `MOVABS` literal is a site row (byte offset, kind, target = the callable row's host entry address, `src/compiler/native/hir-word.f:62,574-578`, or the literal's kind); callee identity is resolved later through the capture's xt->record index (`aot-capture.f:86-100,137-150`), so `hir-word.f` is not changed; `MOVABS` literal rows use the `SNAP-RELOC:MOVABS` kind (cross-build obligation (3): the equality pin in `test/x86-64-seam.f` stays until that kind is defined once); spans exact for return, tail, no-return and `does>` bodies; in shadow mode the call displacement is emitted as zero and the row is authoritative; an emission linked at two placements executes identically on the ThinkPad.
Files: `src/compiler/native/emit-x64.f`, `test/compiler/x64-emit.f`, `test/x86-64-peer-routines.f`.
Verify: spark `bin/hb --load test/compiler/x64-emit.f`; ThinkPad: routine images linked at two placements.
Depends: habu-render-x86-trap-33eba82b (C5).
Route: direct.
Ownership: krait (Intel lane).
Claim: unassigned.
