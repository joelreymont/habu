---
title: Add a fingerprint-keyed slot table to lib
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T12:10:30.325603+03:00"
---

Problem: Maki's import speed stack needed four near-identical copies of a chained slot table over a caller's row store (census identities, plan net names, parser net dedup, root identities; /home/joel/Work/maki/.jj-ws/bigboard/src/kicad_read.f and src/kicad_import.f at 49b15fc4) and wrote SLOT-COUNT twice; reported by rowan on 2026-09-12 (message 20260912-090315.234-rowan-7ba6), low priority. Refined by rowan on 2026-09-12 09:31 UTC after the Opus review of Maki's stack: only the sizing rule belongs in Habu. Acceptance: HM:CAP-FOR ( n -- n ) next to HM:CAP-OK in lib/hashmap.f, answering the power-of-two capacity that keeps HM:PROBE's load under 1 for n keys (Maki writes that rule twice with floors 64 and 1024 and no derivation), with the derivation in its comment, one test in lib/hashmap-test.f (boundary values: 0, 1, a power of two, one over), and a docs/stdlib.md line; the chain walks stay in Maki. Files: lib/hashmap.f, lib/hashmap-test.f, docs/stdlib.md. Verify: the new suite on bin/hb, then Maki's four sites replaceable (Maki side is rowan's). Depends: none. Ownership: hazel. Claim: unassigned.
