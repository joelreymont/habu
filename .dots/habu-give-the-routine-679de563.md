---
title: Give the routine frame one owner that reserves the link slot
status: open
priority: 2
issue-type: task
created-at: "2026-08-01T15:24:51.633734+02:00"
---

A routine that calls keeps its caller's return address in slot zero of its own frame; src/compiler/native/select.f writes it there and test/compiler/native-chain-fixture.f CALL-HABU declares a frame of exactly one slot for it. The register allocator also places spill slots from offset zero upward inside the frame the contract declares (src/compiler/native/regalloc.f, N-SLOTS against FRAME-N), so a routine that both called and spilled would hand slot zero to a value on top of the return address. It cannot happen today - the allocator refuses to spill in a routine of more than one block, and a single-block routine that spilled over the link slot is caught by the validator's no-slot-written-twice rule as E-A64RAV-SHARE - but that is two authorities agreeing by luck rather than one owner. Give the frame a layout with one owner: the link slot first, the allocator's slots after it, and the contract's frame size derived from both.
