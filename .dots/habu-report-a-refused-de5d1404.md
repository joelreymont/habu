---
title: Report a refused does> clause at tier 1
status: open
priority: 2
issue-type: task
created-at: "2026-10-10T05:14:29.318767+03:00"
---

Problem: at tier 1 a checked definer whose `does>` clause the checker refuses is not refused as a refused checked body is, under the Habu loop and the engine loop alike (engine ~/.cache/tmp/carl-hb-9751f482/hb). ~/.cache/tmp/carl-gfrest/c/c9-undefined-clause.f (`: MK ( n -- ) create , does> ( -- n ) NOSUCH drop @ ;`) prints `ncomp: cannot compile MK` and `hb: uncaught throw code -8572` (E-NCOMP-VERDICT), rc 67, with no diagnostic; c9-tick-pending.f (`... does> ( -- n ) ['] MK drop @ ;`) reaches the elaborator first, `ncomp: cannot compile MK at [']`, -8651, rc 67. Tier 0 refuses both `E-UNDEFINED: <name>`, rc 70, and tools/check.f refuses the clause `mk;does` E-UNDEFINED at the token. Both scan the clause before its head; tier 1 scans the head first (CHECK-DOES-SPLIT), and the shared checker then admits a clause naming its own pending definer: src/core/checker.f LIVE-BIND asks CK-PENDING-SYM before the using lookup (:12655-12668) while the CK-CLOSE! window (:12445-12452) holds the head's symbol pending, so c9-tick-pending's clause binds BOUND-PENDING, native's elaborator refuses it. A refused checked colon body at tier 1 prints the checker's diagnostic, then `ncomp: cannot compile <name>`, rc 70 (c/g5-i7-checked.f, E-UNMODELED-IMMEDIATE).
Acceptance: at tier 1, under both loops, a checked definer whose clause the checker refuses prints the checker's diagnostic for the clause, then `ncomp: cannot compile <name>`, rc 70, catchable, as a refused colon body does; the elaborator is not reached. A clause's reference to its own definer gets from the shared checker the verdict tier 0 and tools/check.f give it. Both reproducers join test/outer-interpret.f.
Files: src/compiler/native/compiler.f (CHECK-DOES-SPLIT, the clause's verdict), src/core/checker.f (the clause's view of its pending definer), src/habu/definers.f, test/outer-interpret.f.
Verify: native build per docs/gate.md; `bin/hb --load test/outer-interpret.f`; `bin/hb --load test/run.f`.
Depends: none. Worker: worker-max.
Partly superseded: the tier-1 part goes with the one-pass codegen (docs/architecture.md, "The codegen is one pass over the checked events"); the checker part stands. Do not start the tier-1 part.
