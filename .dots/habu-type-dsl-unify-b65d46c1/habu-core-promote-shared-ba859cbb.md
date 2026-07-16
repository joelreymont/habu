---
title: "Core: promote shared option family"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-16T19:32:53.018059+02:00\""
blocks:
  - habu-migration-core-records-77182600
---

Full context: TYPE-FIELD:FIND must return option<field-id>, but lib/adt/option.f is user-loaded and the post-hook compiler prefix has no option family. Loading TYPE-FIELD before OPTION makes the checked effect unresolvable; defining OPTION through the new payload ENUM would be circular because ENUM consumes TYPE-FIELD. Add one compiler-owned shared option<a> algebra family in the post-hook prefix before TYPE-FIELD, with checked OPTION:SOME/NONE constructors and MATCH behavior, native/recovery/fixpoint/snapshot parity, and no public SUMTYPE dependency. Make lib/adt/option.f a loader-only requirement or remove it once all inventories consume the prefix owner; never maintain a second registry or redeclaration. Own the focused core option module/tests and exact load/path/provided/build/pin inventories. Acceptance: TYPE-FIELD:FIND certifies as option<field-id>; existing option<n>/option<nominal> consumers remain born-typed; duplicate redeclaration rejects; rollback/snapshot identity is stable; native bootstrap, recovery, AOT, and full option suites pass. This prerequisite must land before habu-fields-add-shared-6b063c62 can close.
