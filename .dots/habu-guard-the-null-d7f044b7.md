---
title: Guard the null pointer cell against computed stores
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T14:05:06.867974+03:00"
---

Problem: NULL-PTR reads the engine's permanently zero cell at DATA header offset 800 (src/habu/layout.f NULL-PTR-CELL-OFF, since dab2c17e); checked source cannot name the cell (REG-PROTECT marks the record DNAME-INT) but a store through a computed address, data-base 800 + !, can write it, exactly as ADDRESS-CELLS:LOCK-CELL is written on purpose. A settable null pointer corrupts every pointer-to-number conversion in the engine (CDIGEST:NATIVE-SLOT? and every ptr - NULL-PTR subtraction), so it deserves stronger protection than LOCK-CELL: a PROT-GUARD band cell, which costs nothing at runtime because GUARD-SPAN already tests the interval. The first attempt at 0C8 collided with FFI:FFI-REG-LEN-BUF-OFF and moved test/seal.f's band-edge probes; a guarded placement needs a free cell inside or adjacent to band 2 and the two seal probes shifted with it. Acceptance: a checked-source computed store to the null cell exits SEAL-VIOLATION (83) in a fixture, NULL-PTR still reads zero in the engine and in a stripped image, test/seal.f and test/internal-word-gate.f green, byte fixpoint. Files: src/habu/layout.f, src/core/pointer-storage.f, test/seal.f, test/internal-word-gate.f, bootstrap/cg/forth.fs if the mirror carries the band. Verify: the fixtures on a rebuilt engine. Depends: none (hardening, after release). Ownership: hazel line. Claim: unassigned.
