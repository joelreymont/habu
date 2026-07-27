---
title: Package error-code lint core
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-27T08:57:58.815205+02:00\""
---

Wall 10: tools/error-code-lint-core.f unpackaged - 4 findings from the intern rename (371 lines, 72 defs, 2 consumers: error-code-lint.f entry and its test). Own it: one package, short tails, case-insensitive collision check, consumers migrated same-commit, load-position rule respected. Acceptance: whitespace probe passes; error-code-lint-test green and the production lint runs green on the tree; both diff lints.

Closure correction (2026-07-27, measured at checkpoint): 79 definitions, not 72 (6 constants, 6 create, 13 variable, 54 colon words; 77 carry ECL-, the exceptions are ERROR-CODE-LINT and ERROR-CODE-LINT-STRICT). THREE consumers, not two: the exact-token owner-resolved sweep found test/gate-stdlib-lint-tools.f:122 calling ERROR-CODE-LINT-STRICT inside a GSI-RUN quotation - master-gate infrastructure, already package GATE-LINT-TOOLS, so the migration there is one qualified token with no new package. All three consumers migrate in the same commit. Approved design: package ERROR-CODE-LINT, whole body private; public surface three EXPORT words SCAN-COUNT / SCAN-COUNT2 / STRICT (all probed clean; bare COUNT is unusable - dictionary folds onto core count); the zero-external-reference ledger view ERROR-CODE-LINT becomes a private word (LEDGER, probed clean). Collision oracle (engine-dictionary probe, proven on known-positives first) reports COUNT and U. as the only colliding tails; I and J are reserved. Baseline green: production lint 1267 file(s), 711 claim(s), 32 reservation(s), 0 finding(s) rc=0; whitespace probe reds E-PACKAGE-OWNERSHIP at tools/error-code-lint-core.f:65:3.

DELIVERED IN-LANE (not on master): lane commit 049518c5 "Package error-code-lint core and its callers" creates package ERROR-CODE-LINT with the three-word export surface STRICT / SCAN-COUNT / SCAN-COUNT2, migrates all three consumers in the same commit, and keeps all four ledger counters byte-identical. That commit is not reachable from master; it lives only in the vecmem lane workspace .jj-ws/habu-pkg-vecmem.

Claim: agent=vecmem-lane workspace=.jj-ws/habu-pkg-vecmem (delivered in-lane at 049518c5; codex re-review and integration pending)
