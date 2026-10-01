---
title: Give LVF a band outside the virtual stack
status: closed
priority: 1
issue-type: task
created-at: "\"2026-10-01T04:19:38.356145+02:00\""
closed-at: "2026-10-01T12:50:20.149629+02:00"
close-reason: Landed as c380d4b6 (r4-lvf 6c5284e5); Fable review ACCEPT
---

Problem: src/habu/layout.f LVF-OFF $2C0 (the DO-level array of loop-entry frame bytes, one cell per level) lies inside VVAL-STACK, VVAL-OFF $250 plus VSMAX 32 cells = $250..$350: $2C0 is virtual-stack slot 14. A definition holding 15 or more values on the tier-0 virtual stack inside a counted loop that uses `leave` overwrites one with the other. Reduced (copies in ~/.cache/tmp/kestrel-r4/lvf15.f, lvf1.f): `: F ( n -- n ) {: a:n :} 0 3 0 do 1 2 … 15 + + … + i 1 = if leave then loop a + ; 100 F . cr` crashes rc 134 with 15 literals and stops 'transfer immediate out of range' rc 75 with 16, on d40cc36d and on change yrykozmu. src/habu/data-claims.f:20-30 keeps LVF out of the claims table because CLAIMS-ASSERT would refuse the overlap. Acceptance: LVF gets its own band that overlaps nothing and becomes a DATA-CLAIMS row (the stated exception goes); the reduced cases print their sums at tier 0 and tier 1; the Gforth seed mirror (bootstrap/cg/forth.fs layout constants) moves with it; tests through the real load path seen to fail first. Files: src/habu/layout.f, src/habu/data-claims.f, src/habu/habu2.f readers, bootstrap/cg/forth.fs, a loop test. Verify: loop suites, test/data-claims-build.f, native build convergence, two-generation build, Gforth check-only recovery. Depends: habu-run-a-negative-2fcf85b9 (it moved the LV bands). Ownership: LVF's band.
