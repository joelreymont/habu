---
title: Own stdlib tail schedule
status: open
priority: 1
issue-type: task
created-at: "2026-07-23T05:06:54.486290+02:00"
blocks:
  - habu-pkg-stdlib-gate-fb28fe63
---

Why: verify-source PRIM/PPRIM parity cannot become a real regression while the canonical standard-library tail inventory is an unowned global routine. Required result: package the existing tail inventory in place as package TAIL-PROCESS and publish exactly RUN ( -- ). RUN retains the canonical ordered 22-row inventory, existing group header and test setup, subject and tail-ratchet setup, pool reset, enqueue order, single drain, copy-on-write isolation, timeout, and failure propagation; append test/verify-prim-test.f exactly once. Update only the three production callers to TAIL-PROCESS:RUN and register verify-prim once in the existing standard-library suite inventory so the existing suite-coverage scanner proves the same fork-include row. Keep all scheduler helpers and state private at their existing owners. Do not add a file, alias, duplicate scheduler, scanner exception, copied inventory, compatibility global, or new helper state. Prerequisites: habu-pkg-stdlib-gate-fb28fe63. Owned result and files: the tail block in test/gate-stdlib-inline-lib.f, caller updates in test/gate-runner-lib.f, test/run-worker-stdlib.f, and test/gate-tail-process.f, the verify-prim row in test/gate-stdlib-cases.f, and FILEMAP.md only if required by the new scheduled path. Acceptance: only TAIL-PROCESS:RUN resolves; former global GSI-TAIL-PROCESS and qualified private TAIL-PROCESS names reject; verify-prim executes through the production tail schedule; remove, comment, string, wrong-verb, duplicate, and reorder mutations fail structurally; actual tail-process CLI, resident worker identifier 36, and full standard-library gate preserve behavior. Smallest owning-path check: the focused verify fixture followed by test/gate-tail-process.f and the runner tail-process entry point, with exact package, typed-local, suite-coverage, file-map, and host checks. Claim: unassigned.
