---
title: Consume registry events in package lint
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T17:33:52.090432+02:00"
blocks:
  - habu-add-raw-primitive-da071c0b
---

Files: tools/package-diff-lint-core.f and tools/package-diff-lint-test.f only, after LINT-LEX emits REGISTRY rows. Replace the rejected secondary PRIM/PPRIM grammar with direct handling of the lexer REGISTRY event. Registry bodies cannot open packages, definitions, or execute definer-looking tokens; no prepass, duplicated raw parser, token-count heuristic, or path exemption. Fail closed on any lexical row error before diff ownership analysis. Acceptance: the exact src/core/checker.f primitive-decoder diff is clean; the exact comment-only src/core/sumtype.f diff is clean; malicious rows cannot hide a following global definition or embedded package/definer tokens; malformed, nested, missing-header, wrong-closer, and end-of-input rows reject; ordinary declaration rows still enforce package ownership. Verify: package-diff-lint focused suite with mutation that restores token scanning, exact real-diff fixtures, lint-tools gate, typed-local-diff-lint, host-lint, filemap-lint, dot-dep-lint.
