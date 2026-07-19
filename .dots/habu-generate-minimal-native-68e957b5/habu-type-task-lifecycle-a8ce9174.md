---
title: Type task lifecycle state
status: open
priority: 2
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
  - habu-synchronize-task-lifecycle-f9b01bbf
created-at: "2026-07-19T21:11:11.276689+02:00"
---

lib/task.f:17-21 models the closed lifecycle {empty,constructed,running,done,halt-requested} as raw constants. TASK-STATE@/! expose n, the TCB declares STATUS as an untyped CELL (:29-46), and ACTIVATE/KILL/DONE? compare raw codes through repeated branches (:344-398). The generated AArch64 trampoline embeds raw ordinals 2 and 3 through TASK-RUNNING/TASK-DONE (:297-302). After lifecycle synchronization and the owned TCB STRUCTURE migration, declare a package-owned state ENUM, make the TCB status field and all Forth accessors typed, and dispatch lifecycle behavior with exhaustive MATCH over one transition snapshot/transaction. Keep ordinal encoding only in named trampoline ABI conversion helpers; assert the emitted immediates map to the declared variants and no raw n enters ordinary lifecycle APIs. Preserve empty=0 zero initialization, exact TCB offsets/size, task behavior, machine-code trampoline bytes except required synchronization instructions, and platform ABI. Add checker negatives for n/foreign-enum status writes, exhaustive transition-table tests for every state/event pair, invalid transition rejection, encoded-ordinal parity, and CODELEN/DATA proof. Files: lib/task.f, task-test.f, docs/threads.md. Depends: habu-synchronize-task-lifecycle-f9b01bbf, habu-libs-migrate-runtime-c2738a4d, and habu-checker-capability-layout-4e7f1f03. Ownership: lifecycle type/domain only; memory ordering and transition ownership remain the predecessor.
