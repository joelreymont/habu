---
title: Use pre- and post-indexed forms for stack moves
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-16T16:09:22.607831+03:00\""
---

Problem: a data-stack push compiles as `str x0,[x19]` + `add x19,x19,#8` and a pop as `sub x19,x19,#8` + `ldr x0,[x19]`, and every non-leaf word saves the link register as `sub sp,sp,#16` + `str x30,[sp]` and restores it as `ldr x30,[sp]` + `add sp,sp,#16` (measured 2026-09-16: tier-0 `: TWICE dup + ;` is 7 instructions of which 5 are that frame; tier-1 QUAD is 6 of which 4). ARM64 has single-instruction pre- and post-indexed forms: push `str x0,[x19],#8`, pop `ldr x0,[x19,#-8]!`, save `str x30,[sp,#-16]!`, restore `ldr x30,[sp],#16`. Acceptance: both tiers emit the indexed forms for pushes, pops and the link-register frame; the A64ASM encoders gain the forms with encoder tests; the frame is emitted only for words that call (tier 0 emits it for leaves today); the sample words shrink accordingly (tier-0 TWICE from 7 to 5 or fewer); baked code bytes and engine size before and after; byte fixpoint; full gate green; the crash classifier and debugger breakpoint handler (docs/debugging.md, `sub sp,#16` emulation in EMIT-TRAPH) updated to the new frame shape; the Gforth seed mirrors the tier-0 change (two-stage rule). Files: src/arch/arm64/asm.f, src/habu/habu2.f, src/habu/crash.f, src/compiler/native/emit.f, select.f, bootstrap/cg/forth.fs, bootstrap/cg/crash.fs, docs/debugging.md. Verify: encoder tests; tools/jitdump.f on the sample words; test/engine-stack-debugger.f; tools/native-build.f fixpoint; test/run.f; tools/bootstrap.sh. Depends: none. Ownership: code generation. Claim: agent=hazel-word-frame workspace=.jj-ws/hazel-word-frame.
Design note (hazel, 2026-09-16, Joel: "dot and fix"): the JIT is single-pass, so
it cannot know at `:` whether the word will call anything. Backpatch at `;`:
emit one pre-indexed save `str x30,[sp,#-16]!` at entry; if no bl/blr was
emitted in the body, rewrite it to nop and emit `ret` alone, otherwise emit
`ldr x30,[sp],#16 ; ret`. Leaf: 2 instructions executed instead of 5; caller:
3 instead of 5; no code moves, so addresses recorded during the body stay
valid. Note for the record: 3,566 of the engine's 5,374 baked words are the
boot prefix compiled at tier 0 (docs/compiler-measurements.md), so tier-0
codegen quality is the engine's own speed, not only the REPL's.
Parked 2026-09-16 17:12 (Opus session limit): workspace .jj-ws/hazel-word-frame on 83829f06, no commit, working copy clean; the worker was verifying indexed-form encodings against objdump before its first checkpoint. Resume from scratch with the dispatch brief of 16:47 (encoders, JIT backpatched frame, tier-1 forms, crash/debugger/seed in step).
Landed 2026-09-16 20:50 (duplicates of 70797ed8, a94edda1, a8a63fd4, f4a45c9f): indexed-form encoders with an objdump-verified suite; tier-0 frame only for words that call (backpatched at ;, one nop for leaves), pushes and pops in the transfer; tier-1 link frame fused in the emitter with the two-instruction fallback above 255-byte frames (30 of 9,068); primitives' push/pop fused. Census: tier 0 -20.9 percent bytes and instructions, tier 1 -5.9 percent, primitives -10.7 percent; B == C; the chain caught and the lane fixed a missed second call emitter in the seed. Follow-ups: data-stack move fusion in combine, writeback forms in the insn proof. Closes on the next green batch gate.
