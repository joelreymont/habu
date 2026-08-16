---
title: Chain load-time installers need boot-run rows
status: open
priority: 2
issue-type: task
created-at: "2026-08-16T06:38:19.805553+02:00"
---

Final milestone blocker, found by bake-chain-12 (2026-08-16) - and it fails LOUDLY by name now: the merged engine exits 76 'checker: no source-tape observer to arm' (checker.f:11154). The chain has three top-level installers; only DKEEP-HOOK is on the boot-run list. feed.f:216 INSTALL (CHECKER-TAPE:INSTALL) and clobber.f:193 WATCH-INSTALL (CODE-RECLAIM:WATCH) run at load time inside the window and never run in a seeded engine; both write PREFIX state so no captured cell carries them. Class bounded at exactly these two (bake-chain-12 surveyed every top-level effect in src/compiler/native/). Fix shape already ruled (e98b03d4 MERGE CHANNEL RULINGS 2, the DKEEP-HOOK precedent): move each to public, tools/aot-chain-capture.f declares them on the boot-run list with its existing refusal discipline. Acceptance: the milestone transcript green - tools/aot-chain-bake.f engine, echo s" : FOO ( n -- n ) 1 + ;" 1 1 8 NMIGRATE:DEFINE 7 FOO . prints 8 exit 0, in the registered suite; a mutation dropping either row reds by name.

BOUND CORRECTED + RULINGS 2026-08-16 (bake-chain-13; the dot's
"exactly two" is refuted three ways - source over the real
43-file closure, the live WATCHERS 0->3 registry count, and
habu2.f:5334's own comment): FOUR installers, not two -
NFEED:INSTALL, NCLOB:WATCH-INSTALL, NINL:WATCH-INSTALL,
NPUB:WATCH-INSTALL. The missed two (inline.f, publish.f) would
have shipped a SILENT miscompile: their watchers keep stage/
publish rows above the FORGET floor, and a row that outlives its
code is a body spliced in place of the routine it meant to call.
RULING 1: declare all four, each moved public per the DKEEP-HOOK
precedent. RULING 2: the audit's reader is ONE new public word
AOT-CAPTURE:TRAPPED-BELOW ( b0 b1 d0 -- n ) - aot-capture.f owns
the cell readers and the XTCELL walk; a second private copy in
the tool is the duplication the tree refuses. The invariant it
carries (proven by construction, not arithmetic: 6 pre-window
declared cells hold window addresses - the two triples - and an
extra above-window watcher moves other 89->90 while holding-
window stays 6): EVERY pre-window declared address cell holding
a window code address must be re-established by the boot-run
list; refuse by name otherwise. Plus the resolves-as-the-seed-
asks check on each declared name (XREF-FIND-INDEX = the seed's
qualifier path), catching the private-name class that dies a
silent $52. The pre-window DATA byte-diff audit is REFUTED
(215288 changed bytes of permanent false positives) and stays
dead. The milestone case lands in aot-chain-capture-suite after
PROBE-ARTIFACT (the 6.5s bake is cheap enough).
