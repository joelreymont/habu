---
title: Package set-test lint fixture
status: open
priority: 2
issue-type: task
created-at: "2026-07-27T08:57:58.793013+02:00"
blocks:
  - habu-own-nominal-linear-491d11e4
---

Wall 8 (intern gate, corrected count): tools/lint/set-test.f is unpackaged - 13 E-PACKAGE-OWNERSHIP findings from the intern rename (138 lines, 24 defs, leaf test with no consumers). Own it: one package, short tails with a CASE-INSENSITIVE collision check including lowercase core words (the repl-lint lesson: stripped tails shadowed c!, I, J - reserved and core names need a role prefix). Acceptance: whitespace probe passes; set-test green; both diff lints.

DELIVERED IN-LANE (not on master): lane commit d3b39bef "Package lint set-test suite". That commit is not reachable from master; it lives only in the vecmem lane workspace .jj-ws/habu-pkg-vecmem.

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
