---
title: result cache closure stops at src members; dead candidate-key branch
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:47:07.037191+02:00"
---

Problem: test/run-files.f:44-48 'src/ members are keyed but not scanned' - RUN-CLOSURE does not follow load edges out of src/arch/arm64/disasm.f or src/habu/aot-closure.f (neither carries require lines today, so the hole is latent - the 750d7ee7 class returns the day one does, and test/run-closure-lint.f will not notice); only phases 6 and 8 are keyed (test/run-lib.f:1380-1385) and the candidate-sha branch TR-RESULT-UNDER-KEY? (:1387-1392) is dead (neither is PHASE-UNDER?) while :1359-1363 documents it. Acceptance: RUN-CLOSURE scans src members (or asserts they have no edges, with a fixture that adds one and reds); the dead branch and its doc removed. Files: test/run-files.f, test/run-lib.f, test/run-closure-lint.f, test/run-result-cache-test.f. Verify: run-result-cache-test with the new fixture. Depends: none. Ownership: result cache. Claim: unassigned.
