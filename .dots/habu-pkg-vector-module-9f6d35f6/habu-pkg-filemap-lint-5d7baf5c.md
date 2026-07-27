---
title: Package filemap lint tool
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-27T08:57:58.808184+02:00\""
---

Wall 9: tools/filemap-lint.f unpackaged - 6 findings from the intern rename (254 lines, 49 defs, 1 consumer filemap-lint-test.f). Own it: one package, short tails, case-insensitive collision check, consumer migrated same-commit. Acceptance: whitespace probe passes; filemap-lint runs green as a GATE on the tree (it is a master gate - prove the gate invocation unchanged); both diff lints.

DELIVERED IN-LANE (not on master): lane commit 2b87f9df "Package filemap-lint and its fixture suite" creates packages FILEMAP-LINT and FILEMAP-LINT-TEST with a ten-word export surface, and the production gate is byte-identical (1107 paths, 0 findings, same invocation). Three tails kept their FM- prefix on measured evidence rather than habit: FM-I is a reserved name, and FM-PATHISH?, FM-EXISTS? and FM-U. collide with live words. That commit is not reachable from master; it lives only in the vecmem lane workspace .jj-ws/habu-pkg-vecmem.

Claim: agent=vecmem-lane workspace=.jj-ws/habu-pkg-vecmem (delivered in-lane at 2b87f9df; codex re-review and integration pending)
