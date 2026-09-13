---
title: Complete source-built stage runtime and callee closure
status: active
priority: 2
issue-type: task
created-at: "\"\\\"2026-09-13T11:05:27.080398+03:00\\\"\""
blocks:
  - habu-track-retained-jit-1dc23a17
  - habu-preserve-complete-addr-258c0288
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: unassigned.

Own habu2.f cold-prefix tables/EM-AOT-PATCH-SITES diagnostics (including both complete newline payloads), bootstrap.sh source assembly, stdin/stage source and codegen fixtures; exclude tier dispatch/driver phases. Strict effects already fixed604f2c17. Reconcile pendingb728e382: load/provide stdlib once, mirror prefix rows, remove duplicate errors/prelude from SRC_COMMON. Resolve actual target callees before emitting; constants use supported literal lowering, helpers actual closure definitions, missing names/sites refuse early. Eight unresolved seed names do not justify fake providers/general inliner. Verify aot-wide-format/aot-wid-restore/build-fixpoint-fixtures and assembly differences. Supported recovery repair is not current-native selfbuild prerequisite.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.

Cedar reduction (private cedar-source-prefix): the pending stdlib-prefix patch called LAPPPROV in a baked-source product, but EMIT-COLD-PREFIX-SHARED defined that callee only for STDIN products. Gdb-launched hb-stage stopped at0x401a94 executing BL0x401a94, with the preceding ADR naminglib/prelude.f; this was an infinite self-branch, not slow compilation. Factoring the two source-append callees so both source modes emit them advances source certification and stage fixpoint promptly. The maker then reaches the recorded E-UNDEFINED:true in the next source-built product; the full stdlib/callee repair remains open. No debugger attachment permissions or system configuration were changed. Plain NF-REPL fixture exit74 after reading its full prefix/stdin is also reported by the argv lane; path construction/open succeeds and that source-stage behavior still needs reduction.
