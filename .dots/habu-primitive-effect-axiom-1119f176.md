---
title: Primitive-effect axiom table + difftests
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T22:54:40.831796+02:00"
---

Consolidate scattered primitive effect assertions (PRIM: rows in src/core/checker.f:2167-2216 PES table + TRUST rows on engine primitives) into ONE audited axiom table - the explicit, minimal trust root for typing. Each axiom gets a differential test: execute the primitive on generated stacks and compare observed depth/value behavior against the declared effect (extend test/prop-test-core.f machinery once habu-unfreeze-checker-prop lands). Deliverable: docs section naming the axiom set + gate suite proving every axiom has a difftest; inventory ratchet counts axioms separately from discharged TRUSTED.

## Census green again (2026-07-06, on 8c97e26b)

The census had gone red at head: 13 axioms landed unclassified (item-12
substrate accessors wf-n@/wf-tokix@/wf-pos@/wf-fam@/wf-width@, tfam-n@/
tfam-width@, sumv-n@, tf-str-u@, tf-pk-n@, schema-n@/schema-root-n@, plus
seal-capture from the truncation-seal work). Classified per implementation
evidence, not rubber-stamped:

- GEN (difftested): the seven zero-arg high-water readers (wf-n@, tfam-n@,
  sumv-n@, tf-str-u@, tf-pk-n@, schema-n@, schema-root-n@) — pure `variable @`
  reads (checker.f:4691, type-family.f:189-191,373, type-schema.f:111,131),
  same class as ndict@/cp@; verified executing clean at top level.
- NOEXEC (new AX-NOEXEC-C): wf-tokix@/wf-pos@/wf-fam@/wf-width@ and tfam-width@
  fail closed with `76 die` on an out-of-range index (WF-ROW@ checker.f:4688,
  TF-REC@ type-family.f:184-186), so a dummy operand can kill the census
  process; seal-capture (native BSEALCAP habu1.f:1725) rewrites the sealed
  friend-band watermark — a live-state mutator like cp!/ndict!.

`./bin/hb 1 5 < test/prop-test.f` rc 0: 210 axioms, 106 difftested, 104 noexec,
0 unclassified, 0 mismatches. docs/effects.md noexec taxonomy updated to name
the fail-closed-accessor and seal-capture classes. Remaining scope of this dot
(fold engine-primitive TRUST rows into the audited table + difftests) is
untouched and stays open.
