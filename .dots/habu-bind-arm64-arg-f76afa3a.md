---
title: Bind ARM64 argument and result registers
status: open
priority: 1
issue-type: task
created-at: "2026-07-31T00:29:29.442967+02:00"
---

Design section 7.9 asks the allocator to respect fixed register constraints and lists unmet fixed constraints among the things its validator rejects. src/compiler/native/regalloc.f has none, and it has none for a reason: the straight-line A64IR subset has no way to say that a value must be in a particular register. Design section 7.6 says an externally callable Habu word receives its inputs and publishes its outputs through canonical data-stack slots, and the dialect has no load, no store and no way to name the data-stack pointer, so there is nothing yet that could pin a block argument or a returned value to a place. Until that exists a block argument is just a value that has to be somewhere and the allocator gives it the next free register of the routine's declared set, which is why every value in a routine contract's result set is currently left alone. When the calling-convention seam lands, give A64IR a way to carry a fixed-register constraint on an operand or result, have the allocator pre-colour those intervals before the scan and refuse a program whose fixed constraints cannot all be met, and have the validator check every constraint independently. Acceptance: a block argument declared to arrive in a named register is allocated there; a program whose fixed constraints conflict is refused by name; an assignment that ignores a declared constraint is refused by the validator. Owners: A64IR, A64RA, A64RAV, and whichever leaf owns the Habu word calling convention. Depends on habu-allocate-straight-line-bc4e0075.
