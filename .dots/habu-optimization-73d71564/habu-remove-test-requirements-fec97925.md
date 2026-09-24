---
title: Remove obsolete JIT inliner code and fixture requirements
status: closed
priority: 2
issue-type: task
created-at: "2026-09-13T14:51:13.076942+03:00"
closed-at: "2026-09-16T14:34:48.374561+03:00"
close-reason: "done: The dead C-CALL scan and copy helpers and the fixtures that required them are gone. [No CARRY-SITE or EMIT-OUTSIDE anywhere in src, tools or test; test/addrmap-call.f and test/p2-map-rewind.f remain as behavioural cases.]"
blocks:
  - habu-track-retained-jit-1dc23a17
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: cedar, .jj-ws/cedar-closure-identity.

Own tools/c-call-emitter-test.f helper-retention assertions, obsolete p2-map-rewind/addrmap-inline cases and habu2.f dead scanner/copier removal only if pending tier stack has not already removed them. Decision374c95ff is delete. Audit callers; preserve current BL, address relocation and rollback behaviors with live test vehicles. KEEP real/call/loop/quotation-spill cases survive. Verify emitter/native/map suites and full gate. Do not count definitions/comments as behavior or duplicate tier deletion.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.

Implementation removes the uncalled C-CALL scan/copy helpers, CARRY-SITE,
EMIT-OUTSIDE and its emitted label/body; direct BL emission and the live escape
copy handlers stay intact. The source-shape fixture requiring those dead helpers
is retired. addrmap-call now checks exact direct targets and real storage/value
behavior without copied-body or frame-size assumptions. p2-map-rewind finds the
actual emit target, preserving stale-site and neighbour checks when narrow dup
itself is a call. B product passes both fixtures; the rebuilt combined product
and full native gate are still required before this leaf closes.
