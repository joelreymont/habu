---
title: Declare the leftover pointer cells in tools/check-core.f
status: active
priority: 2
issue-type: task
created-at: "2026-09-18T02:35:11.613116+03:00"
---

Problem: tools/check-core.f CHK-HB-A and seven CHK-*-BUF-A cells are raw cells holding pointers that the lib/tools conversion (habu-convert-the-raw-8052992f) could not reach because the file was blocked behind src/habu/verify-source.f on the rule engine; the engine-source lane (2026-09-18) unblocked it and scratch-patched the eight cells to prove its build. Acceptance: the eight cells declared (TYPED-VARIABLE NAME ptr t or PTR-VARIABLE), wrappers removed, the file loading on ~/.cache/hazel/engines/raw-rule-gen2 without E-RAW-CELL-PTR and tools/check.f's suites green. Files: tools/check-core.f. Verify: raw-rule-gen2 --load tools/check.f; test/run.f. Depends: habu-convert-the-raw-7b5ce4f3 landing. Ownership: tools. Parent: habu-refuse-a-ptr-5ad2734e. Claim: agent=hazel-raw-test workspace=.jj-ws/hazel-raw-test.
