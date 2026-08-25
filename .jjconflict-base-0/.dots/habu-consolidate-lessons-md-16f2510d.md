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

FIVE GROUPS DONE 2026-08-17 (audit-close), NOT CLAIMED - the dot
stays open for the rest. Done means the whole group: one canonical
entry carrying its dates as a list, and every member deleted from
its dated block with that block's count and numbering repaired.
  BOTH contradictory pairs, which were the risky part:
    schedule-lint  4 members -> one entry under Gate Harness, all
                   three readings kept and the tension stated
                   (reachable-by-a-slice is not runs); the
                   2026-08-15 library-entry-point false green is
                   the third reading and belongs with them.
    lldb           2 members -> one entry under Tool & Infra,
                   split by seeded vs ordinary image.
  never-edit-while-running   4 members -> one entry (a census
                   counts as a run; the widen and do landings each
                   had a member).
  pgrep-in-a-wait-loop       3 members -> one entry.
  pipeline/substitution RC   4 members -> one entry (two of them
                   were in the topical half).
Blocks retitled and renumbered: widen four->three, tail
two-and-a-strengthening->two, no-return four->three, merge
four->three, wordlist four->two, caller-naming pair->find,
bake-chain-22 -> the lane's eight. Two blocks went entirely (gate
hygiene 2026-08-10, the do landing 2026-08-13) because every
member was in a group above. Net -85 duplicated lines, +60 of
canonical text.

Claim: agent=lessons-close workspace=.jj-ws/habu-lessons-close (RELEASED 2026-08-21: workspace gone, no live lane - gc)

THIRTEEN MORE GROUPS DONE 2026-08-17 (lessons-close), 74 members ->
13 canonical entries. NOTE the leaf did NOT carry a banked survey:
the members were not line-listed anywhere, so this lane re-derived
them from a full read of all 7411 lines.
  probe-the-leaf            13 members -> one entry (## PROBE THE
                            LEAF), dates 08-01..08-17.
  stale-binary/seeded       10 -> one entry under Tool & Infra
                            (both halves; install --force, seed
                            refresh, control build of master).
  load-sensitivity          10 -> one entry under Gate Harness
                            (contention reds, per-RUN temp roots,
                            the assert-number discriminator).
  dump-the-record            9 -> one entry under Diagnostics.
  mutation-never-ran         6 -> one entry (compile, reach the
                            assertions, unique anchor, green
                            baseline, delete don't move).
  surviving-mutation         6 -> one entry (seam / redundant /
                            no-producer / already-caught).
  restates-its-own-subject   6 -> one entry (the file's own "most
                            repeated mistake"; folds the vacuous-
                            proof and census-strengthening rules).
  generated-ctor-names       3 -> one entry under Types (TF-CTOR-ESC).
  refusal-list               3 -> folded into the i/j-locals five.
  memo-dominance             2 -> one entry.
  git-apply-in-a-workspace   2 -> one entry.
  per-record-walk            2 -> one entry (span blindness + a
                            recorded length is not an extent).
  break-and-watch            2 -> one entry (near-verbatim pair in
                            adjacent bake-chain-22 blocks).
Every counted heading was renumbered to match its items (verified
mechanically: 209 headings, 0 count/sequence mismatches). Both
contradictory pairs were left exactly as the previous lane merged
them - verified by read-back, tension still stated. No content
loss: every distinctive token on all 579 removed lines was checked
against the new file; the 9 that changed spelling were confirmed
by hand to keep their fact. Blocks 779 -> 756; file shrank 7411 ->
7338 lines.

REMAINING: roughly a dozen smaller groups this lane did not reach,
all of them real but none named by the survey's counts:
registration-is-not-execution (passing/scheduled/suite-registration
/TEST:SUITE-is-not-runs, ~4 members); the E-USING-SHADOW-GLOBAL /
public-tail-vs-global family (~5, spans both halves); arena-slot
exhaustion from throwing fixtures (~5); typed-local-diff-lint reads
only added lines (3); a local shadows a word / loop index (4);
jj destructive ops need a validated target (~5); one-fact-one-word
/ two authorities drift (~5); never overwrite a live bin/hb (2).
The file is in no half-merged state - every group above is complete.
