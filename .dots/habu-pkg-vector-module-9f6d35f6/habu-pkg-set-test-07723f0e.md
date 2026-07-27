---
title: Package set-test lint fixture
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-27T08:57:58.793013+02:00\""
---

Wall 8 (intern gate, corrected count): tools/lint/set-test.f is unpackaged - 13 E-PACKAGE-OWNERSHIP findings from the intern rename (138 lines, 24 defs, leaf test with no consumers). Own it: one package, short tails with a CASE-INSENSITIVE collision check including lowercase core words (the repl-lint lesson: stripped tails shadowed c!, I, J - reserved and core names need a role prefix). Acceptance: whitespace probe passes; set-test green; both diff lints.

DELIVERED IN-LANE (not on master): lane commit d3b39bef "Package lint set-test suite". That commit is not reachable from master; it lives only in the vecmem lane workspace .jj-ws/habu-pkg-vecmem.

Claim: agent=vecmem-lane workspace=.jj-ws/habu-pkg-vecmem (delivered in-lane at d3b39bef; codex re-review and integration pending)
