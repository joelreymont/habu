---
title: Lower spills and reloads in the ARM64 dialect
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-31T00:29:17.865285+02:00\""
---

A straight-line block can hold more values at once than any register file has, so register pressure is real and cannot be proved away: a long chain of literals reaches it. src/compiler/native/regalloc.f therefore refuses such a program by name, E-A64RA-PRESSURE, rather than assigning a frame slot, because the A64IR dialect has no store, no load and no frame-slot operand record - an allocation that said a value lives in a slot would name an instruction nothing can emit. This is the same shape as the selector refusing trapping arithmetic that has no machine lowering. To turn the refusal into a decision the dialect needs a memory form and a frame-slot operand, and src/compiler/a64-effect.f already owns the rule that decides whether a slot is addressable at all, A64EFF:CHECK-SLOT, plus the frame region a routine declares. Add the store and load forms and the frame-slot operand to src/compiler/native/a64ir.f, then give the allocator a spill choice with a cost, and give the validator the matching check: every spilled value has a slot inside the declared frame, no two live values share a slot, and every reload reads the slot its value was written to. Acceptance: a block whose pressure exceeds the pool allocates with spills instead of being refused; a spill slot outside the declared frame, an unaligned slot, and two live values in one slot each reject by name; the refusal E-A64RA-PRESSURE is removed or narrowed to the case where the frame itself is exhausted. Owners: A64IR, A64RA, A64RAV. Depends on habu-allocate-straight-line-bc4e0075.

Claim: agent=spilllane workspace=.jj-ws/habu-lower-spills-and-ef14a0dd
