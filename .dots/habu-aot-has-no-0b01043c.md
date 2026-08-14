---
title: AOT has no relocation class for pre-window DATA literals
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-10T18:56:46.262335+02:00\""
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

RULED 2026-08-12 (prewindow design lane, full measurement report in the
orchestrator session; instrument patch preserved in the session scratchpad
as pw-census-instrument.patch): ELIMINATE THE CLASS, KEEP THE REFUSAL FATAL.
The carry options are REFUTED by measurement: the metabuild host truncates
its boot dictionary (hide.f truncates back to SEQ, the FIRST prefix file)
and recompiles the ENTIRE core prefix a second time without rewinding DP, so
every pre-window address a window word can hold lives in a 4.91MB band with
NO counterpart in the target - and the two layouts are not even
order-isomorphic (include.f lands after layout-buffer-seal.f in the host,
before it in the target), so no delta or monotone map exists. A verbatim
carry is silent corruption (the target range is mapped, writable, empty).

THE IMPLEMENTATION THIS DOT NOW MEANS: during an AOT capture, C-CALL
declines to inline a body carrying a recorded address chain whose value lies
below d0, and emits the BL instead - the BL is already relocated correctly
by name by shipped machinery. Structural test at the copy site (same total
classification ACAP-SCAN-DSITES performs); makes pre-window DATA empty BY
CONSTRUCTION; aot-capture's fatal refusal stays as the backstop. Needs a
small piece of compiler state (capture-active + d0 visible to C-CALL) -
design that first. Both reachability probes are in the lane report (one
create + one reference reaches the class; rc 74 named refusal fires).

RIDERS (own dots): pre-window CODE literals (['] on a prefix word) are
CARRIED, not eliminated - a name-keyed row kind on the existing call-site
table, built into the widening pass (recorded on 089f5faf's leaf); J-IS
re-pointing a prefix defer from window code is today blocked only by a
certification accident (habu-answer-j-is dot); the fixpoint stamp does not
key the boot prefix (habu-stamp-the-prefix dot). The intern move 567d8484
is necessary AND SUFFICIENT for NSTR provided the chain compiles entirely
inside the window (one side of d0). The 181 below-d0 XTCELL rows are NOT a
carry class (the target declares its own defer cells) - removed from this
dot's class list.

Claim: agent=prewindow workspace=.jj-ws/habu-prewindow
