---
title: Model dead paths in the elaborator
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T00:23:52.563452+02:00"
---

The checker knows throw/die/CTL-DEAD end a path (checker.f:7105 DEAD-CUR?, MATCH-SEMI relies on it); the elaborator compiles throw as an ordinary call, so a branch ending in throw fails the join width check: ': JT ( n n -- n ) 0 = if drop E-A-EMPTY throw then ;' - checker accepts, chain refuses E-NELAB-JOIN (-8503). Costs 19 census refusals today and is a PREREQUISITE for MATCH: 57 of 303 match sites (19%) have a throw/die arm. Fix: the elaborator's walk marks a path dead when it stages a call to a word the checker certifies as CTL-DEAD (read the certified effect - one authority, no second list), and a dead path neither joins nor contributes width. Acceptance: JT compiles and runs; a dead arm inside if/else joins correctly; a body ALL of whose paths are dead publishes with no join; the 19 E-NELAB-JOIN census refusals drop to their non-dead remainder; adversarial fixture: a word NAMED throw that is not CTL-DEAD still joins. Files: src/compiler/native/elaborate.f. Verify: native-elaborate/native-chain, census, full gate. Depends: none. Blocks: the MATCH lane.
