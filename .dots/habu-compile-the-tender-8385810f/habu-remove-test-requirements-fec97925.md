---
title: "Remove obsolete JIT inliner code and fixture requirements"
status: open
priority: 2
issue-type: task
created-at: "2026-09-13T14:51:13.076942+03:00"
blocks:
  - habu-track-retained-jit-1dc23a17
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: unassigned.

Own tools/c-call-emitter-test.f helper-retention assertions, obsolete p2-map-rewind/addrmap-inline cases and habu2.f dead scanner/copier removal only if pending tier stack has not already removed them. Decision374c95ff is delete. Audit callers; preserve current BL, address relocation and rollback behaviors with live test vehicles. KEEP real/call/loop/quotation-spill cases survive. Verify emitter/native/map suites and full gate. Do not count definitions/comments as behavior or duplicate tier deletion.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.
