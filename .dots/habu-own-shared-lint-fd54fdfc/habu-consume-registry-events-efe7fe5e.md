---
title: Consume registry events in package lint
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-22T17:33:52.090432+02:00\""
closed-at: "2026-07-26T08:59:33.259819+02:00"
close-reason: "Implemented, reviewed, merged: landed as fe092cd2a976 (Consume lexer registry events in package lint), an ancestor of master@origin. The package lint handles the REGISTRY event directly, fails closed on lexical row errors before ownership analysis, and the exact checker.f and sumtype.f real-diff fixtures are clean; the E-DIFF-SYNTAX mislabel for malformed registry rows is fixed with the negative fixture. Deviation amended in the dot text: dispatch amendment item 32 (verify-source row scanners onto LINT-LEX plus the char-operand differential fixture) did not land and is carried by the fail-open row-scanner migration dot minted in this wave."
blocks:
  - habu-add-raw-primitive-da071c0b
---

Files: tools/package-diff-lint-core.f and tools/package-diff-lint-test.f only, after LINT-LEX emits REGISTRY rows. Replace the rejected secondary PRIM/PPRIM grammar with direct handling of the lexer REGISTRY event. Registry bodies cannot open packages, definitions, or execute definer-looking tokens; no prepass, duplicated raw parser, token-count heuristic, or path exemption. Fail closed on any lexical row error before diff ownership analysis. Acceptance: the exact src/core/checker.f primitive-decoder diff is clean; the exact comment-only src/core/sumtype.f diff is clean; malicious rows cannot hide a following global definition or embedded package/definer tokens; malformed, nested, missing-header, wrong-closer, and end-of-input rows reject; ordinary declaration rows still enforce package ownership. Verify: package-diff-lint focused suite with mutation that restores token scanning, exact real-diff fixtures, lint-tools gate, typed-local-diff-lint, host-lint, filemap-lint, dot-dep-lint.

Amended at closure (2026-07-26): the dispatch amendments recorded on
2026-07-25 landed in part. Landed: the package lint consumes the lexer
REGISTRY event directly, and the core diagnostic mislabel is fixed - a
malformed registry row is no longer rethrown as the diff-parser code
E-DIFF-SYNTAX (tools/package-diff-lint-core.f records that rule), with the
negative fixture for the old malformed row shape. NOT landed: amendment item
32 - src/habu/verify-source.f row scanners never became LINT-LEX REGISTRY
consumers (the file has no LINT-LEX reference), and the char-operand
differential fixture does not exist. That remaining work is carried by the
follow-up dot minted in this metadata wave for migrating fail-open row
scanners onto the shared lexer.

Claim: agent=claude-solo workspace=.jj-ws/habu-consume-registry. Recorded
retroactively at closure; implemented during the solo-orchestrator shift.
