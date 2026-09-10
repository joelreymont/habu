---
title: Complete native quotation and control-flow support
status: open
priority: 1
issue-type: task
created-at: "2026-09-10T18:03:13.346260+03:00"
---

Owner: Cedar; src/core/checker.f quotation row instantiation and src/compiler/native/elaborate.f quotation/local/control handling. Existing per-call instantiated arity and stored-quotation compilation changes are in frozen e9fd7e6d; generic-call, native-catch, native-quot and native-stored-quot focused tests passed. Remaining concrete reducers: /tmp/cedar-generic-quotation-reuse.f binds a quotation local row to the first ambient prefix and rejects a second application; native-leave/again/j and locals-scope expose quotation-local/nested-control gaps. Correct the responsible semantics, including return-stack and alias paths identified by the native suite; replace obsolete negative tests with executed positive programs only where support is real. Integrate independently reviewed nominal-pointer change 1eb56c73 (currently applied in cedar-image child, not yet rebuilt there); both original Maki reducers pass in cedar-pointer. Acceptance includes repeated quotation calls under different prefixes, lexical locals/nested control, and rejected incompatible rows. Related existing dots: habu-bind-a-locals-923668b9 and habu-multishot-quotations-typed-8832cace.

New concrete peer reducers: /home/joel/Work/maki/build/lifecycle-product-validate-repro.f and lifecycle-product-repro.f show inferred six-cell PRODUCT locals losing their bundle after a second locals group/repeated use, E-NELAB-UNDER or E-NELAB-BUNDLE; the frozen peer host executes both. pointer_review owns a separate fix from fd9342de. Kestrel reducer /home/joel/Work/radar/build/handoff-task.f fails existing TASK-RUN-USER at opaque TCB.USER-XT @ catch with E-NELAB-QUOT; migrate the actual task callback storage/effect, preserving exception behavior. String-test STR-PARSE-SOME currently fails E-NELAB-MATCH on the strict image, pending combined cleanup integration and reduction. Preserve explicit run-in-stack [ -- ] migration and target-specific abs call assertion in native-dstack-alias.
