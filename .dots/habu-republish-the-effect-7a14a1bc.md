---
title: Republish the effect when a standalone does> patches a word
status: open
priority: 2
issue-type: task
created-at: "2026-10-27T15:02:50.415423+02:00"
---

Found reviewing the alias stamp fix (jerry-stamp .out/rev p3/p5/p8/p9, 2026-10-27), predates it, both tiers: a standalone does> (no create in the defining word) never republishes the patched word's effect. `3 constant KC  : BEHAVE ( -- ) does> ( n -- n ) drop 1+ ; BEHAVE  : W0 ( -- n ) KC ;` is admitted and `5 6 W0 . .` prints `7 5`: W0 consumes a cell it does not declare. A second standalone does> ( n n -- n ) leaves the word typed ( n -- n ). Aliases: the publish step clears guard facts only on LASTC, so an EXPORT between create and does> (reachable via a trusted evaluate helper, p8) keeps the alias's old effect; the correct use is refused (p9). Checker admits wrong programs: fix the does> publish (checker row + guard facts) for the patched record and every alias.
