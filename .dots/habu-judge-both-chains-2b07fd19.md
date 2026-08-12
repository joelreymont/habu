---
title: Judge both chains on one shared corpus
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-05T10:36:19.649103+02:00\""
---

Claim: agent=judge workspace=.jj-ws/habu-judge-corpus

MEASURED 2026-08-12, master 326b1216 - two claims below are OUT OF DATE and the
lane is not to act on them:

- "CALL-FAN-BIG 88-vs-36" is now a DRAW at 40 bytes against 40. The committed
  tables say so (test/compiler/codegen-compare-baseline4.txt:72,
  test/compiler/codegen-chain-baseline4.txt:90) and a live run agrees.
  src/compiler/native/combine.f folded each copied `3 * 5 +` into one
  multiply-add and the trade closed.
- The known-loss name exemption is EMPTY: tools/codegen-compare-report.f
  KNOWN-LOSS$ is `s" "`, so every byte loss on every row is already a counted
  finding. There is nothing for its deletion to surface, and it dies with the
  old harness after the cut rather than being reworked now.

What IS still true and is what the lane acts on: the bodies are hand-retyped in
tools/codegen-compare-migrated*.f, the pinned inputs are written out a third
time in tools/codegen-compare-c*.f, and the two rows the chain cannot compile
(CODEGEN-CORPUS4:PRESSURE-LOOP and CALL-PRESSURE) are exempted by NAME through
CODEGEN-GAP - measured here as E-A64RA-SPILL (-8508), which is what replaces the
list.

LANDED SO FAR IN THIS LANE: tools/judge/src.f reads a corpus source file
structurally and derives each definition's text, arity and callees; tools/judge/
chain.f compiles that text through the chain and records the routine or the
MEASURED refusal code; tools/judge.f writes test/compiler/judge-baseline.txt,
which the gate checks byte for byte through tools/judge-test.f. Corpus 4 only,
bytes only.

The cost column landed too: tools/judge/cost.f generates each column's timing
and value body from the row's ONE pinned input text, certified by the checker
and compiled, so a REFUSED subject simply has no body built and the three-way
input duplication (cases4/new4/c4) is gone. Costs are printed under a marker
line and never compared; the check stops at that line. Every generated body is
held against the address of the column it was built for, which is the one way a
body can be wrong that comparing answers cannot see.

CORPORA 1/2/3/5 SWEPT 2026-08-12, master 497e50c2. Every definition of each
corpus handed to the chain through the judge's own reader, with the corpus
package open so private names resolve. Results, subjects only:

- corpus5: all six compile. Nothing new needed.
- corpus3: all ten subjects compile, floats and `ptr a` signatures included.
  Only the FILL-* setup helpers refuse (E-A64RA-POOL), and they are not rows.
- corpus2: TV-NEXT? and T-RES-WALK compile - a `create`d table READ needs
  nothing special. SYM-FOLD-C refuses with E-NFEED-LITERAL (-8405); see below.
- corpus1: CELL-BUMP refuses with E-A64RAV-DKEEP (-8611) under plain DEFINE and
  COMPILES under NMIGRATE:DEFINE-DATA with the data word's spelling, measured.
  A `create`d cell that is WRITTEN needs the data declaration; one that is only
  read does not.

SYM-FOLD-C IS THE FINDING. The corpus writes `$41`, `$5A`, `$20`; the chain
cannot record a hexadecimal literal on its tape (dot
habu-record-the-engine-79c570ed) and refuses the corpus's own program. The old
harness's column does not hit that because it RESPELLS the body in decimal -
tools/codegen-compare-migrated2.f:100 writes 65, 90 and 32, and its head says
so at line 28 - so the row reads green today while the chain cannot compile the
text the engine compiled. Under the judge that is a REFUSED row with -8405 and
that dot. It is the hand-copy divergence this leaf predicted, found by
measurement rather than by reading.

STILL OPEN, IN ORDER: (1) the four corpus judge files. Needs JUDGE-SRC to also
read `create`/`variable` declarations and report which data words each body
names, JUDGE-CHAIN to pick DEFINE-DATA when there is exactly one (and refuse,
named, when there is more than one - the migration entry takes one), and
publication to run inside the corpus's package with derived names qualified for
the size reader, the way tools/codegen-compare-migrated.f does it. (2) the
seeded-random differential oracle over generated straight-line bodies, which is
what replaces the finite recorded vectors, with the shared-corpus-text blind
spot stated in the artifact header.

CG-27 + CG-28, transition evidence. Old subjects live in tools/codegen-compare-corpus*.f while new subjects are hand-copied strings in tools/codegen-compare-migrated*.f with inputs/results repeated; the gate compares finite recorded vectors and its own tests fabricate comparator rows. Corpus 4 compiles 11/13 rows, and name-based known-loss/unsupported exemptions (compare-report.f:519-575,625-633,675-679) let the gate exit clean despite CALL-FAN-BIG 88-vs-36 and two uncompiled rows. Fix: compile one canonical source artifact through both chains and judge against an independent semantic oracle or property set; committed gaps and size losses are explicit failures or explicit raw measurements, never name exemptions; add adversarial inputs (MIN-INT/MAX-INT overflow boundaries, seeded random bodies, spill-pressure words). After the cut, delete the old-vs-new harness and keep the oracle, a compact production corpus, the new chain's committed baseline, and the optional clang reference.
