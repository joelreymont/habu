---
title: Package repl-lint core module
status: open
priority: 2
issue-type: task
created-at: "2026-07-27T08:47:43.780839+02:00"
blocks:
  - habu-own-nominal-linear-491d11e4
---

Sixth unpackaged-legacy wall instance (intern checkpoint): tools/repl-lint-core.f is unpackaged, so the intern migration's edits to four of its definition bodies (ADD-REPL-PATH, PATH-ID<, COLLECT-REPL-PATHS, REPL-LINT-CHECK) trip E-PACKAGE-OWNERSHIP. Contained, measured: 72 definitions, 2 consumers (repl-lint.f entry, repl-lint-test.f). Own the module: one package, short tails, both consumers migrated same-commit, load-position rule respected. Acceptance: whitespace-edit probe passes after; repl-lint-test green; both diff lints.

DELIVERED IN-LANE (not on master): lane commit 08747978 "Package repl-lint core and its callers". The real closure was four files, including tools/repl-lint-test-lib.f, and the public surface came out at three words. That commit is not reachable from master; it lives only in the vecmem lane workspace .jj-ws/habu-pkg-vecmem.

PARKED 2026-07-27. The vector lane is stopped at a clean boundary and this
contract is not dispatchable. Two independent destruction reviews rejected the
work it rests on. The six-blocker vector verdict (blackboard message
20260727-155303.315-codex-9253 on channel habu-extend-typed-vector-320e1620)
found that the public typed interface still takes a bare pointer, so arbitrary
byte storage is accepted as a vector header and no vector owner or element
identity exists; that disposal clears capacity and length before a fallible
release, so a refused unmap makes retry a no-op and leaks the mapping; and that
the closed-predicate premise behind the typed search is false. The seven-blocker
interner verdict (blackboard message 20260727-154724.143-codex-da26 on channel
habu-pkg-intern-lint-e735c0f6) found that the chunk append copies and advances
before it reserves, that lazy initialization is non-recoverable, that the fault
tests do not prove allocator failure, and that chunk ownership is erased into
three independent vectors with no rollback or disposal lifecycle. Any lane
commit named above is preserved as rejected evidence in
.jj-ws/habu-pkg-vecmem; none of it is work to resume. This dot now blocks on
habu-own-nominal-linear-491d11e4, the design parent that has to freeze the
nominal linear vector owner first, and it may not be re-dispatched until that
design review is clean.

Claim: RELEASED 2026-07-27 with the park above. The vecmem lane worker is released and .jj-ws/habu-pkg-vecmem is kept as rejected evidence only.
