---
title: Delete unreferenced files
status: closed
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:30.968079+03:00"
closed-at: "2026-10-15T17:22:06.516448+03:00"
close-reason: Deleted sha-check.f, fs-path.f, test/mmap-exhaust.f, examples/llm/bad.f, tools native-bootstrap/data-residue-census/row-shape-probe/callable-arity-probe after rg showed no reference; good.f header corrected; public-signatures-bracket-test ok.
---

Problem: files with no require, include, registry row or caller: src/core/sha-check.f (19 lines); src/lib/fs-path.f; src/lib/test/mmap-exhaust.f; src/examples/llm/bad.f (its BAD5 claim is false: u8 to u32 widens and rc=0 was demonstrated; stale Gforth narration); tools/native-bootstrap.f (26); tools/data-residue-census.f (58); tools/row-shape-probe.f (211); tools/callable-arity-probe.f (117). src/examples/llm/good.f stays (it is the subject of test/gate-diagnostics-lib.f PUBLIC-SIGNATURES and tools/public-signatures-bracket-test.f), but its header is corrected. test/host-io-image.py moved to the Python-harness dot (it covers behavior no gated test covers); the roles.f and util.f duplicates moved to the dead-engine-words dot (engine prefix and seed). Acceptance: rg shows no reference, then each file deleted; good.f header corrected. Files: those named. Verify: rg; tools/public-signatures-bracket-test.f. Depends: none. Ownership: listed files. Claim: unassigned.
