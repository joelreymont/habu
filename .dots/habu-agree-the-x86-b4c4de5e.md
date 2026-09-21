---
title: Agree the x86-64 data-stack stand between selector and validator
status: active
priority: 2
issue-type: task
created-at: "2026-09-21T12:50:36.086456+03:00"
---

Problem (found by the allocator-vocabulary lane, test/compiler/x64-regalloc.f header): an x86-64 routine under the DATA-STACK convention (X64ABI:LEAF) is refused by A64RAV with E-A64RAV-DSTACK at regalloc-verify.f VDPLACE-CK. The validator re-derives where the data-stack pointer must stand from A64SEL's placement policy - stand where most boundary transfers name - while X64SEL stands at the entry base, taking all argument bytes at entry and publishing at exit (select-x64.f, INTEL.md 'Selection, slice A'). No vocabulary states the policy; it is a selector/validator agreement. Acceptance: the stand policy becomes a fact the dialect states (a vocabulary field or a routine-level attribute the selector writes and the validator reads), both selectors state theirs, VDPLACE-CK checks against the stated policy instead of re-deriving A64SEL's, the x64-regalloc suite gains the LEAF (data-stack) allocation case with ACCEPTED? true, and the ARM64 cases are unchanged in what they assert; three generations, gen2 == gen3. Files: src/compiler/native/regalloc-verify.f, dialect.f, select-x64.f, select.f (only if the policy is written per routine), test/compiler/x64-regalloc.f. Verify: native-regalloc, x64-select, x64-regalloc, test/run.f. Depends: habu-bind-the-register-623e83ff. Ownership: hazel. Claim: agent=hazel workspace=.jj-ws/hazel-x64-stand.
