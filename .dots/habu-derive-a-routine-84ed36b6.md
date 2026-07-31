---
title: "Derive a routine's frame from what its spills need"
status: open
priority: 2
issue-type: task
created-at: "2026-07-31T20:09:03.305859+02:00"
---

src/compiler/native/regalloc.f now decides spills and refuses by name (E-A64RA-PRESSURE) when the frame the routine contract declares has no room for the next slot, and it publishes A64RA:FRAME-USED - how much frame the walk actually used. Nothing computes the contract's frame from that: every caller today passes a frame in (the suites pass 16 or 32 because they counted the spills by hand), so a routine whose body needs one more slot than its author guessed is refused rather than given the frame it needs. The frame is a field of A64EFF:routine and a routine contract is declared before its body is allocated, so the fix is a two-pass shape - allocate against a trial contract, read FRAME-USED, and re-declare the contract with the frame the program proved it needs - or a contract whose frame field is filled in by the allocator. Whichever it is, the routine that reaches the emitter must declare exactly the frame its reserve takes, which A64RAV already checks. Owners: A64RA, A64EFF. Depends on habu-lower-spills-and-ef14a0dd.
