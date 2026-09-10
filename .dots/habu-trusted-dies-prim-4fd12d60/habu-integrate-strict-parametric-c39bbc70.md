---
title: Integrate strict parametric effects and checked Forth helpers
status: open
priority: 1
issue-type: task
created-at: "2026-09-10T18:03:13.337312+03:00"
---

Owner: Cedar. Resume the original checked-Forth work preserved in the root workspace and its WIP strict-parametric parent (current working change zpxzqzzp; parent change xvrrkxtp, earlier revision 07f62c5c). Do not discard the root changes in lib/table.f, lib/task.f, lib/task-test.f, native-exec/native-rename-rows tests, bundle/diagnostic/event helpers and Unicode class parsing. Integrate strict non-specialization of declared quantifiers, typed pointer views, checked DEFTYPE casts and combinators against the new native runtime; correct declarations where implementations specialize values. Acceptance: legitimate polymorphic programs run through the real load path, attempts to manufacture nominal types reject, and each converted helper has a checked body rather than a replacement PRIM assertion. Existing related work: habu-multishot-quotations-typed-8832cace and habu-builder-trust-rows-c5d41af6. Source ownership: strict effect/type rules and their ordinary Forth consumers; preserve unrelated peer changes.

Current integration: cedar-source ntuttpnq on fd9342de includes the preserved strict checker/caller sweep. Cold native build13 passed using the private crossing seed; bin/hb now contains strict parametric checks. Added genuine zero-code byte-view and cell-view primitives with precise pointer effects instead of the unsound generic empty BYTE-VIEW colon. Read-only independent design review agrees; regression test/pointer-view.f and typed TBL accessor tests are running. Ordinary integer/label-cell/record effects corrected through the native build path; table scalar APIs preserve pointee types. Broad suite and final review remain pending. Fresh allocation stays generic and does not use CELL-VIEW.
