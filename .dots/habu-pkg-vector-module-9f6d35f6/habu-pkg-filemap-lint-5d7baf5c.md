---
title: Package filemap lint tool
status: closed
priority: 2
issue-type: task
created-at: "\"2026-07-27T08:57:58.808184+02:00\""
closed-at: "2026-08-02T16:54:45.213422+02:00"
close-reason: superseded by 85a9646fd6b97e5d2cbb86d637bcf8d8ab2aece8; its sole owned filemap lint and fixture were deleted
blocks:
  - habu-own-nominal-linear-491d11e4
---

Wall 9: tools/filemap-lint.f unpackaged - 6 findings from the intern rename (254 lines, 49 defs, 1 consumer filemap-lint-test.f). Own it: one package, short tails, case-insensitive collision check, consumer migrated same-commit. Acceptance: whitespace probe passes; filemap-lint runs green as a GATE on the tree (it is a master gate - prove the gate invocation unchanged); both diff lints.

DELIVERED IN-LANE (not on master): lane commit 2b87f9df "Package filemap-lint and its fixture suite" creates packages FILEMAP-LINT and FILEMAP-LINT-TEST with a ten-word export surface, and the production gate is byte-identical (1107 paths, 0 findings, same invocation). Three tails kept their FM- prefix on measured evidence rather than habit: FM-I is a reserved name, and FM-PATHISH?, FM-EXISTS? and FM-U. collide with live words. That commit is not reachable from master; it lives only in the vecmem lane workspace .jj-ws/habu-pkg-vecmem.

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
