---
title: AOT has no relocation class for pre-window DATA literals
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T18:56:46.262335+02:00"
---

Third relocation class (seed-closure lane 2026-08-11, NOT covered by bb9b6d70): NSTR's arena and NTRAP's table are allotted when the PREFIX loads - below every capture window's d0 - so a chain-compiled REPL word holding a string address or trap ordinal-table reference carries a literal that is invisible to the window scan AND would be wrong to rebase by the window delta (its correct value is fixed by the prefix's own DP, which differs between the metabuild host and bin/hb). Needs its own class: either the site records carry a 'prefix-DATA' kind rebased by the prefix delta, or pre-window structures move/are re-derived at boot. Probe first. BLOCKS THE CUT alongside bb9b6d70. Files: src/habu/aot-capture.f + habu2.f seed passes. Depends: habu-per-site-relocation-bb9b6d70 stage 1.

ORDERING, ESTABLISHED 2026-08-11 (aotsite lane): this is a PRECONDITION of
putting the chain in the capture window (habu-seed-the-chain-e98b03d4), not a
post-cut follow-up. NSTR interning is content-keyed, so equal bodies share one
address: as soon as the chain loads with the prefix, a window word that uses
bytes the prefix already interned is handed the pre-window address, which the
capture now refuses by name and which stops the build. Moving NSTR to
here/allot (habu-intern-str-bodies-567d8484) does not avoid it - that only
places bodies interned AFTER the window opens.

