---
title: Split guarded FFI from ABI call body
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T20:32:08.683834+02:00"
---

Measured target-code duplication in src/habu/habu1.f:1681-1809 and bootstrap/cg/forth.fs mirror. BFFI-CALL-ABI-BOUNDED-CORE already guards every live x0..x8 and stack span with BFFI-GUARD-BOUNDS, then serializes seven live registers back through the Forth value stack, including synthetic int-nargs=0 and sret=0, only for BFFI-CALL-ABI-CORE to pop the same seven registers and run BFFI-GUARD-ARGS. The zero values make that 13-instruction, 52-byte raw guard a runtime no-op. Because the bounded core is meta-expanded into both ffi-call-abi-bounded and ffi-call-abi-r-bounded, the image contains two no-op guards plus two seven-value push/pop round trips. G-PUSH and G-POP are two instructions each, so the removable sequence is exactly 168 target bytes per bounded primitive, 336 bytes total: 112 bytes of seven push/pop pairs, 52 bytes of dead guard, and 4 bytes of synthetic zero materialization. The same meta composition also expands the full ABI invoke body separately into raw integer-return, raw float-return, bounded integer-return, and bounded float-return primitives. Root fix: separate pre-call guard adapters from one target-machine ABI invoke body. Raw adapters pop nargs/sret and run the raw band guard; bounded adapters retain their already-validated registers and call the body directly; integer/float return tails remain small adapters. Share the invoke body out of line with a proved register/clobber contract and preserve stack alignment, x20 restoration, sret x8, spill-copy bounds, trap 83, and foreign-call unwind behavior. Proof: emitted disassembly shows no value-stack round trip and no BFFI-GUARD-ARGS in bounded variants, an exact byte ledger accounts for at least the 336-byte structural cut, all four ABI variants keep positive/negative bounds and return-value regressions, bootstrap mirror is byte-equivalent, fresh macOS/Linux fixpoints and size maps lower honestly, full native/AOT/FFI gates pass. Cross-check the active Share primitive bounds guards work so both dots converge on one helper ABI rather than creating competing guard helpers.

The same audit found a second exact expansion in src/habu/habu1.f:1738-1774:
BFFI-CALL-N-CORE emits 34 instructions, 136 bytes, and is copied into both
ffi-call-n and ffi-call-bounded. One framed target helper plus two direct call adapters
can share that integer-only spill/call/restore body. Include its exact before/after
ledger and keep raw versus bounded guard ownership in the adapters. This is separate
from sharing BFFI-GUARD-BOUNDS itself: the duplicated payload is the ABI spill loop,
register loads, foreign branch, restoration, and result push.
