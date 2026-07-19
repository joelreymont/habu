---
title: Separate stdin closure lint tests
status: open
priority: 3
issue-type: task
created-at: "2026-07-19T20:53:33.708506+02:00"
---

tools/stdin-closure-lint.f mixes the lint implementation, command entrypoint, and its only fixture. SDCL-SELFTEST is compiled and executed on every lint invocation, reads a real repository file, and proves only that CONTAINS? returns true for one present token and false for one absent token. There is no stdin-closure-lint-test.f, so missing entries in each consumer, unreadable/over-cap files, long consumer names, manifest role filtering, finding aggregation, and command exit behavior have no isolated mutation coverage. Factor package STDIN-CLOSURE-LINT core words into a reusable core module, leave the CLI file as a minimal run/report entry, and move the detector proof plus per-consumer mutation matrix into a focused test file scheduled by the lint-tools suite. The production lint should perform only the real scan once; the test must use private fixture files and prove each missing closure dependency produces the exact named finding and nonzero exit. Files: tools/stdin-closure-lint-core.f, tools/stdin-closure-lint.f, new test, suite inventory, FILEMAP.md. Depends: none. Ownership: stdin-closure lint structure/fixtures only; no closure manifest or engine build changes.
