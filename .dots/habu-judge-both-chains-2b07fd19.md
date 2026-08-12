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

CG-27 + CG-28, transition evidence. Old subjects live in tools/codegen-compare-corpus*.f while new subjects are hand-copied strings in tools/codegen-compare-migrated*.f with inputs/results repeated; the gate compares finite recorded vectors and its own tests fabricate comparator rows. Corpus 4 compiles 11/13 rows, and name-based known-loss/unsupported exemptions (compare-report.f:519-575,625-633,675-679) let the gate exit clean despite CALL-FAN-BIG 88-vs-36 and two uncompiled rows. Fix: compile one canonical source artifact through both chains and judge against an independent semantic oracle or property set; committed gaps and size losses are explicit failures or explicit raw measurements, never name exemptions; add adversarial inputs (MIN-INT/MAX-INT overflow boundaries, seeded random bodies, spill-pressure words). After the cut, delete the old-vs-new harness and keep the oracle, a compact production corpus, the new chain's committed baseline, and the optional clang reference.
