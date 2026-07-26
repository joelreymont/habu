---
title: Shorten run-lib private stems
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T19:39:12.108210+02:00"
---

Follow-up to habu-pkg-test-run-ee2e47da, the habu-shorten-reserved-lint-cf8a0038 precedent shape: after package TEST-RUN owns test/run-lib.f, the ~348 private TR-* definitions remain migration debt obscuring the package-local vocabulary. Rename private TR-* stems to short package-local tails, update only internal references, keep the public surface (the 67 names the packaging commit publishes) unchanged. One definition per concept; no aliases, no new exports, no behavior change - diagnostics and gate output byte-for-byte identical. Acceptance: no TR-* definition or reference remains in the file; public dictionary surface unchanged; test/run.f full-output byte comparison identical; both diff lints. Owner: package TEST-RUN. Dependencies: habu-pkg-test-run-ee2e47da landed; not on the critical path - spare capacity only.
