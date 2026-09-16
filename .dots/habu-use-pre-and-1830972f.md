---
title: Use pre- and post-indexed forms for stack moves
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T16:09:22.607831+03:00"
---

Problem: a data-stack push compiles as `str x0,[x19]` + `add x19,x19,#8` and a pop as `sub x19,x19,#8` + `ldr x0,[x19]`, and every non-leaf word saves the link register as `sub sp,sp,#16` + `str x30,[sp]` and restores it as `ldr x30,[sp]` + `add sp,sp,#16` (measured 2026-09-16: tier-0 `: TWICE dup + ;` is 7 instructions of which 5 are that frame; tier-1 QUAD is 6 of which 4). ARM64 has single-instruction pre- and post-indexed forms: push `str x0,[x19],#8`, pop `ldr x0,[x19,#-8]!`, save `str x30,[sp,#-16]!`, restore `ldr x30,[sp],#16`. Acceptance: both tiers emit the indexed forms for pushes, pops and the link-register frame; the A64ASM encoders gain the forms with encoder tests; the frame is emitted only for words that call (tier 0 emits it for leaves today); the sample words shrink accordingly (tier-0 TWICE from 7 to 5 or fewer); baked code bytes and engine size before and after; byte fixpoint; full gate green; the crash classifier and debugger breakpoint handler (docs/debugging.md, `sub sp,#16` emulation in EMIT-TRAPH) updated to the new frame shape; the Gforth seed mirrors the tier-0 change (two-stage rule). Files: src/arch/arm64/asm.f, src/habu/habu2.f, src/habu/crash.f, src/compiler/native/emit.f, select.f, bootstrap/cg/forth.fs, bootstrap/cg/crash.fs, docs/debugging.md. Verify: encoder tests; tools/jitdump.f on the sample words; test/engine-stack-debugger.f; tools/native-build.f fixpoint; test/run.f; tools/bootstrap.sh. Depends: none. Ownership: code generation. Claim: unassigned.
