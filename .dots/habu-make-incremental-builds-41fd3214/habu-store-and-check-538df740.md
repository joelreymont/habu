---
title: Store and check each dependency row once
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T14:59:32.104581+03:00"
---

Problem: a warm engine build verifies 1,683,427 dependency rows (515 MB, 0.97 s in NUNIT-DEP:CHECK, tools/native-unit-dependency.f) and reads 602 MB of hb-def entries whole; RESOLUTION rows are 1,139,810 / 332 MB but only 174,442 (15.3%) are distinct, the rest are per-definition copies re-verified every build. Cold bookkeeping around each compile (dependency recording, export, pack, publish) is ~7.2 s. Evidence: ~/.cache/tmp/carl-prof/logs (warm 6.8 s / cold 85 s engine build profile, master 60ad9461). Acceptance: each distinct row is stored once (shared, content-addressed beside the hb-def entries) and checked at most once per build against the live dictionary; definition entries reference rows; cache size and warm row-check time drop by the duplication factor (measure before/after: cache bytes, NUNIT-DEP:CHECK time, cold bookkeeping split); hit/miss behaviour of every existing cache E2E unchanged; a row change still misses exactly its dependents. Files: tools/native-unit-dependency.f, tools/native-definition-cache.f, tools/native-unit-object.f. Verify: test/native-file-storage-e2e.f, test/native-file-auto-e2e.f, engine cold + memory.f comment-edit builds with counts and bytes. Depends: habu-redesign-def-storage-4e595333, habu-key-definitions-on-34aed0d9 (brief 2 changes row kinds). Needs a Plan design first. Claim: unassigned.
