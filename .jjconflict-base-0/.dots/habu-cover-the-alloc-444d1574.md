---
title: "Cover the allocator's frame ceiling"
status: open
priority: 3
issue-type: task
created-at: "2026-08-06T15:03:13.993166+02:00"
---

PROBLEM: src/compiler/native/regalloc.f NEW-SLOT still refuses with E-A64RA-PRESSURE, but the meaning changed under habu-derive-a-routine-84ed36b6: it is no longer 'the declared frame ran out' (the frame is derived now) but 'the demand passed FRAME-CEIL', the tighter of A64EFF:FRAME-MAX and NFROZEN:VMAX slots. The case that used to exercise it (test/compiler/native-regalloc.f SMALL-FRAME) now proves the derivation instead, so the ceiling has NO test. It is close to unreachable: a walk cannot spill more values than the module holds and no module holds more than NFROZEN:VMAX, so only a routine whose prologue owns a slot AND that spills all 256 can pass 2048 bytes. ACCEPTANCE: either a fixture that builds a module wide enough to reach the ceiling and pins E-A64RA-PRESSURE, or a decision that the ceiling belongs to A64RAV alone (it already refuses a slot ordinal at NFROZEN:VMAX with E-A64RAV-SLOT) and the allocator's check is retired. Do not fake it with a lowered constant - that would test the fixture, not the chain. Files: src/compiler/native/regalloc.f, test/compiler/native-regalloc.f. Depends: none. Ownership: A64RA. Claim: unassigned. Found by the spillwire lane 2026-08-06.
