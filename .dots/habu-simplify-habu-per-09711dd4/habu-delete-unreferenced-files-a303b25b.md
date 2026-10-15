---
title: Delete unreferenced files
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:30.968079+03:00"
---

Problem: files with no require, include, registry row or caller: src/core/sha-check.f (19 lines); src/lib/fs-path.f; src/lib/test/mmap-exhaust.f; src/examples/llm/bad.f (its BAD5 claim is false: u8 to u32 widens and rc=0 was demonstrated; stale Gforth narration); test/host-io-image.py (225-line orphan Python harness, mentioned only in prose at docs/swiftforth-task-api.md:61); tools/native-bootstrap.f (26); tools/data-residue-census.f (58); tools/row-shape-probe.f (211); tools/callable-arity-probe.f (117). roles.f:174-186 VRDEF-FOLD-C and VRDEF-STR=CI duplicate util.f CORE-*; util.f:125 RDP is unused. src/examples/llm/good.f stays (it is a fixture for test/gate-diagnostics-lib.f:1375 and tools/public-signatures-bracket-test.f:50), but its header is corrected. Acceptance: re-confirm no reference with rg, then delete each file; roles.f uses util.f CORE-*; RDP deleted; prose references updated. Files: those named. Verify: rg shows no reference; the suites that load roles.f and util.f; tools/examples-test.f. Depends: none. Ownership: listed files. Claim: unassigned.
