---
title: Consolidate LESSONS.md overlap
status: open
priority: 2
issue-type: task
created-at: "2026-08-16T18:58:41.830372+02:00"
---

Deferred twice, now dotted: the 2026-08-13/14 blocks overlap (probe-the-leaf x3, stale-binary x2, generated-strings x2) and 2026-08-15/16 added ~20 more blocks with recurring themes (pipeline-RC twice, load-sensitivity twice, per-record-walk blindness, mutation-earns-deletion x2). One consolidation pass: merge duplicates into single canonical entries, keep dates as a list per entry, no content loss. Run AFTER the bake-chain-15 landing (its lessons append to the same file - conflict avoidance). Text-only; gates: none beyond reading it back.

SURVEYED 2026-08-17 (audit-exec) AND NOT CLAIMED - the survey is
the deliverable here, because it says the dot is three times the
job it was written as. Released for a lane that can carry it.

THE COUNT IS WRONG IN THE DESCRIPTION ABOVE. A full read found
THIRTY duplicate groups, not seven, and the named ones are all
bigger than they look:
  probe-the-leaf              8 members, not 3
  stale-binary                7, not 2
  dump-the-record             11, not 2
  load-sensitivity            8, not 2
  mutation-earns-deletion     6, not 2
  pipeline-RC                 6, not 2
  generated-strings           4, not 2
  per-record-walk blindness   3
Plus 22 groups nobody had listed, including four near-verbatim
pairs where the second member adds nothing: the literal-memo
dominance pair (5693-5713), the refusal-list restatement
(6114-6133 vs 6138-6148), the pgrep-in-a-wait-loop trio (which
labels itself a repeat twice), and never-edit-during-test/run.f
(four members, three near-verbatim).

TWO PAIRS ARE CONTRADICTORY AND MUST NOT COLLAPSE INTO ONE CLAIM.
The schedule-lint pair: 5674 says trust the lint over "I added a
SUITE block"; 7251 shows the lint answers a different question
than the runner does (it reads the phase-to-slice map and never
asks TEST:PHASE-RESIDENT?). And the lldb pair: 6949 gives a
working recipe (--stop-at-entry then br set -a); 7021 says neither
software nor hardware breakpoints fire on a SEEDED engine. The
merge has to keep the refutation and the seeded/unseeded split.

AND THE FILE HAS TWO HALVES. Topical sections run 186-4068 and the
per-lane blocks 4069-7366, dated from 5162. Pipeline-RC,
load-sensitivity and stale-binary all have members in BOTH, so a
pass that only touches the dated blocks leaves the topical copies
behind and the duplication survives.

WHAT THE NEXT LANE SHOULD DO: one canonical entry per theme with
its dates as a list, every distinct fact preserved (the survey
records what each member says that its siblings do not), the
duplicate members deleted from their dated blocks, and the
chronology kept for entries that are unique. Text-only; read it
back and check the two contradictory pairs by hand.
