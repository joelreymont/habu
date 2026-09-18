---
title: "Store the type registries' names as offsets, not pointers"
status: active
priority: 1
issue-type: task
created-at: "2026-09-18T02:57:37.396941+03:00"
---

Problem: src/core/checker.f CT-NAME-A-P and VREC-NAME-A-P are PERSISTED-PTR-VARIABLE heads of pointer tables into the string pool that REG-PERSIST-MOVE (checker.f:9325) grows with here + allot and repoints with 'dst pvar !'; here is PE-PTR-A-RAW (src/habu/prims.f), so a here-derived pointer can never be ptr ptr u8, and making REG-PERSIST-MOVE pointee-parametric is refused with E-NONPARAMETRIC-EFFECT (prefix-form lane, 2026-09-18, measured); no declared form mints a here-allotted pointer table at run time, so under the raw-storage rule these two sites cannot be declared and a rule-carrying engine cannot build its next generation (the build stops at them). The workaround 'head @ CELL-VIEW 0 ptr-field' is the launder habu-refuse-ptr-field-331a9731 refuses; not taken. Acceptance: the two registries store OFFSETS into the string pool (n cells) instead of pointers, the readers rebuild the pointer from the pool base at use, CT-STR-REBASE and CT-SNAPSHOT-MARK-POINTERS are deleted (offsets need no rebase and no snapshot marking), REG-PERSIST-MOVE keeps its effect, a rule-hosted generation build then succeeds, byte fixpoint, bootstrap check, test/run.f. Files: src/core/checker.f (the CT and VREC registries, REG-PERSIST-MOVE callers), src/habu/snap-lib.f if it marks those pointers, test/. Verify: the rule-hosted build; fixpoint; tools/bootstrap.sh check; test/run.f. Depends: habu-give-the-prefix-cad2bf07 (the declared table forms). Ownership: checker registries. Parent: habu-refuse-a-ptr-5ad2734e. Claim: agent=hazel-reg-offsets workspace=.jj-ws/hazel-reg-offsets.
