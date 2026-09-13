---
title: "Complete source-built stage runtime and callee closure"
status: open
priority: 2
issue-type: task
created-at: "\"2026-09-13T11:05:27.080398+03:00\""
blocks:
  - habu-track-retained-jit-1dc23a17
  - habu-preserve-complete-addr-258c0288
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: unassigned.

Own habu2.f cold-prefix tables/EM-AOT-PATCH-SITES diagnostics (including both complete newline payloads), bootstrap.sh source assembly, stdin/stage source and codegen fixtures; exclude tier dispatch/driver phases. Strict effects already fixed604f2c17. Reconcile pendingb728e382: load/provide stdlib once, mirror prefix rows, remove duplicate errors/prelude from SRC_COMMON. Resolve actual target callees before emitting; constants use supported literal lowering, helpers actual closure definitions, missing names/sites refuse early. Eight unresolved seed names do not justify fake providers/general inliner. Verify aot-wide-format/aot-wid-restore/build-fixpoint-fixtures and assembly differences. Supported recovery repair is not current-native selfbuild prerequisite.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.
