---
title: Report a refused does> clause at tier 1
status: open
priority: 2
issue-type: task
created-at: "2026-10-10T05:14:29.318767+03:00"
---

Problem: at tier 1 a checked definer whose `does>` clause the checker refuses is not refused as a refused checked body is, under the Habu loop and the engine loop alike (engine ~/.cache/tmp/carl-hb-9751f482/hb). ~/.cache/tmp/carl-gfrest/c/c9-undefined-clause.f (`: MK ( n -- ) create , does> ( -- n ) NOSUCH drop @ ;`) prints `ncomp: cannot compile MK` and `hb: uncaught throw code -8572` (E-NCOMP-VERDICT), rc 67, with no diagnostic; c9-tick-pending.f (`... does> ( -- n ) ['] MK drop @ ;`, the Gforth host's case r17) reaches the elaborator first, `ncomp: cannot compile MK at [']`, -8651, rc 67. Tier 0 refuses both `E-UNDEFINED: <name>`, rc 70, and tools/check.f refuses the clause `mk;does` E-UNDEFINED at the token. A refused checked colon body at tier 1 prints the checker's diagnostic, then `ncomp: cannot compile <name>`, rc 70 (c/g5-i7-checked.f, E-UNMODELED-IMMEDIATE).
Acceptance: at tier 1, under both loops, a checked definer whose clause the checker refuses prints the checker's diagnostic for the clause, then `ncomp: cannot compile <name>`, rc 70, catchable, as a refused colon body does; the elaborator is not reached. Both reproducers join test/outer-interpret.f and the Gforth host's test/gforth/cases/.
Files: src/compiler/native/compiler.f (CHECK-DOES-SPLIT, the clause's verdict), src/habu/definers.f, test/outer-interpret.f, test/gforth/cases/.
Verify: native build per docs/gate.md; `bin/hb --load test/outer-interpret.f`; `bin/hb --load test/run.f`.
Depends: none. Worker: worker-max.
