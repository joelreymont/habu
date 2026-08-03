---
title: Fuse a call-boundary reload with the store that puts it back
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T12:24:10.068728+02:00"
---

Measured in the fourth codegen table (tools/codegen-compare-corpus4.f). At every call boundary the chain reloads each live value out of its data-stack slot and immediately stores the same value back into the SAME slot for the next call. Disassembled from CODEGEN-CORPUS4:CALL-FAN-N, which carries ONE live value across five calls and is 34 instructions: sub x19,x19,#8 / ldr x0,[x19] / str x0,[x19] / add x19,x19,#8, six times over - 24 of the 34 instructions, whose net effect on the machine is nothing, because the value the callee reads is already in the slot it is read from and the value it leaves is already in the slot the next callee reads. It scales with the number of live values: CODEGEN-CORPUS4:CALL-LOOP-3-N holds six (index, limit, three locals, accumulator) and emits six ldr followed by six str to the same six addresses between each pair of its three calls, and CODEGEN-CORPUS4:TINY-CALLEE-N holds three and does it four times a turn. This is NOT the same question as habu-narrow-what-a-5d6a0845, which is about saving fewer values; this one is about not moving a value that is already where it belongs, and it would help even after the save set is narrowed. Where it lives: src/compiler/native/select.f CALL-SAVE / CALL-RESTORE, plus the coalescing step in src/compiler/native/regalloc.f that already removes an a64.mov whose two ends get one register. Owners: A64SEL, A64RA.
