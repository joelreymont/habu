---
title: Judge both chains on one shared corpus
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T10:36:19.649103+02:00"
---

CG-27 + CG-28, transition evidence. Old subjects live in tools/codegen-compare-corpus*.f while new subjects are hand-copied strings in tools/codegen-compare-migrated*.f with inputs/results repeated; the gate compares finite recorded vectors and its own tests fabricate comparator rows. Corpus 4 compiles 11/13 rows, and name-based known-loss/unsupported exemptions (compare-report.f:519-575,625-633,675-679) let the gate exit clean despite CALL-FAN-BIG 88-vs-36 and two uncompiled rows. Fix: compile one canonical source artifact through both chains and judge against an independent semantic oracle or property set; committed gaps and size losses are explicit failures or explicit raw measurements, never name exemptions; add adversarial inputs (MIN-INT/MAX-INT overflow boundaries, seeded random bodies, spill-pressure words). After the cut, delete the old-vs-new harness and keep the oracle, a compact production corpus, the new chain's committed baseline, and the optional clang reference.
