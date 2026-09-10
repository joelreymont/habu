---
title: Complete native quotation and control-flow support
status: open
priority: 1
issue-type: task
created-at: "2026-09-10T18:03:13.346260+03:00"
---

Owner: Cedar; src/core/checker.f quotation row instantiation and src/compiler/native/elaborate.f quotation/local/control handling. Existing per-call instantiated arity and stored-quotation compilation changes are in frozen e9fd7e6d; generic-call, native-catch, native-quot and native-stored-quot focused tests passed. Remaining concrete reducers: /tmp/cedar-generic-quotation-reuse.f binds a quotation local row to the first ambient prefix and rejects a second application; native-leave/again/j and locals-scope expose quotation-local/nested-control gaps. Correct the responsible semantics, including return-stack and alias paths identified by the native suite; replace obsolete negative tests with executed positive programs only where support is real. Integrate independently reviewed nominal-pointer change 1eb56c73 (currently applied in cedar-image child, not yet rebuilt there); both original Maki reducers pass in cedar-pointer. Acceptance includes repeated quotation calls under different prefixes, lexical locals/nested control, and rejected incompatible rows. Related existing dots: habu-bind-a-locals-923668b9 and habu-multishot-quotations-typed-8832cace.
