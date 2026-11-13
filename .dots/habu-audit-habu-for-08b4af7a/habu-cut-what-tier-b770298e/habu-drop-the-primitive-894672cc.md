---
title: "Drop the primitive rows' unread stow kind"
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T18:21:34.104385+01:00"
---

Problem: src/habu/prims.f EC2-INIT!/EC2-RECORDS! set a stow kind that the kernel object carries (src/arch/arm64/kernel-object.f CHECK-ROW+), src/arch/arm64/link-kernel.f STATE-ROW passes to CHECK:PRIM-CLOSE and src/core/checker.f PRIM-C2-STOW! stores as PE-C2-INIT/PE-C2-RECORDS, which nothing reads since the C2 stow borrow chain went. Acceptance: the flags, the object field, the argument and the PE bits are gone; hb builds itself; Zen, Tender and Maki build. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-fu6.
