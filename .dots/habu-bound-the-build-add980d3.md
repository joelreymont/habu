---
title: Bound the build maker deadline by CPU
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T21:23:31.378769+02:00"
---

Problem: tools/hb-build-lib.f:45 HBB-MAKER-DEFAULT-MS 600000 and :420-428 HBB-READ-MAKER-TIMEOUT (HB_BUILD_TIMEOUT_MS) give every maker child a wall-clock deadline (:698, :705, :718, :731; src/lib/process.f E-PROC-TIMEOUT), so load alone fails a correct build. Measured 2026-10-29 21:16, carl-cstr stage B, 12 cores at load 150: the engine build died with code=E-PROC-TIMEOUT maker=engine source=src/habu/habu2.f timeout_ms=600000 while the maker (native-build-support, pid 12403) was still working at about 15 percent of a core (1:10 CPU at 7:55 elapsed). Same class as habu-bound-the-lsp-c5cbc535 (gate rows) and the fixed check.f and check-test deadlines. Acceptance: the maker guard is a deadlock guard that a busy child cannot trip: it is bounded by the child CPU time the driver reads, or fires only when the child consumes no CPU for the guard period; HB_BUILD_TIMEOUT_MS goes (habu-remove-env-vars-17febc6a); a host at ten times its core count in load builds the engine to completion with no override; the guard still ends a child that stops consuming CPU. Files: tools/hb-build-lib.f, src/lib/process.f, tools/hb-build.f header, docs that name HB_BUILD_TIMEOUT_MS. Verify: an engine build under a saturated host completes; a fixture child that blocks forever is ended by the guard. Depends: none; the variable removal is coordinated with habu-remove-env-vars-17febc6a. Ownership: build driver. Claim: unassigned.
