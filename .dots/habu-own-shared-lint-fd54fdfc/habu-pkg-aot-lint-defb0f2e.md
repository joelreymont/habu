---
title: Package AOT lint tests
status: active
priority: 1
issue-type: task
created-at: "2026-07-22T17:32:16.714900+02:00"
---

Files: tools/aot-lint-test-lib.f and tools/aot-lint-test.f only. Put the library in package AOT-LINT-TEST, make helpers and buffers private, rename ALT-MAIN to RUN, and change the entry file to call AOT-LINT-TEST:RUN. Do not change the AOT lint core API. Acceptance: no ALT-* definition or storage remains global; clean, bad, JSON, label, and structured outcome assertions remain active; no compatibility alias. Verify: bin/hb --load tools/aot-lint-test.f, typed-local-diff-lint, package-diff focused mutation, host-lint, filemap-lint.

Claim: agent=aot_test_pkg workspace=.jj-ws/habu-pkg-aot-lint-defb0f2e.
