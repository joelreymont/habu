---
title: Package repl-lint core module
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-27T08:47:43.780839+02:00\""
---

Sixth unpackaged-legacy wall instance (intern checkpoint): tools/repl-lint-core.f is unpackaged, so the intern migration's edits to four of its definition bodies (ADD-REPL-PATH, PATH-ID<, COLLECT-REPL-PATHS, REPL-LINT-CHECK) trip E-PACKAGE-OWNERSHIP. Contained, measured: 72 definitions, 2 consumers (repl-lint.f entry, repl-lint-test.f). Own the module: one package, short tails, both consumers migrated same-commit, load-position rule respected. Acceptance: whitespace-edit probe passes after; repl-lint-test green; both diff lints.

DELIVERED IN-LANE (not on master): lane commit 08747978 "Package repl-lint core and its callers". The real closure was four files, including tools/repl-lint-test-lib.f, and the public surface came out at three words. That commit is not reachable from master; it lives only in the vecmem lane workspace .jj-ws/habu-pkg-vecmem.

Claim: agent=vecmem-lane workspace=.jj-ws/habu-pkg-vecmem (delivered in-lane at 08747978; codex re-review and integration pending)
