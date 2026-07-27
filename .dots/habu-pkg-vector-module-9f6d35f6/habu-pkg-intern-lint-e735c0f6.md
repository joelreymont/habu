---
title: Package intern lint module
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-27T00:09:32.400720+02:00\""
blocks:
  - habu-pkg-set-test-07723f0e
  - habu-pkg-filemap-lint-5d7baf5c
  - habu-pkg-err-code-9508f911
  - habu-pkg-repl-lint-cd31219c
  - habu-pkg-public-signatures-e25db8b1
  - habu-extend-typed-vector-320e1620
---

Fifth unpackaged-legacy wall instance, measured: tools/lint/intern.f (191 lines, no package keyword) reds E-PACKAGE-OWNERSHIP on a whitespace-only body edit (intern.f:72 INTERN#), so its 8 raw vector references cannot convert until the file is owned. NOT file-local (checkpoint correction): packaging renames the external surface, and the exact-token consumer set resolved by owner is 7 files, 80 references (set-test 42, error-code-lint-core 12, dot-dep-lint-core 7, filemap-lint 7, repl-lint-core 6, public-signatures-core 3, suite-coverage-lint-core 3) - a naive sweep says 97 across 12, but 17 references in 5 maki/db files are the obligation domain OWN word INTERN (obligation.f:824), a name collision, not consumers: qualify by resolved owner, never by text. ONE commit covering the file plus all seven consumers (split leaves red intermediates; master-always-green forbids it); gate by loading each consumer suite for real; the file is already partly migrated (uses VEC:@, defines its own INTERN>INDEX bridge) and carries a documented residual identical to sched-key's, so its vector-call conversion onto the typed surface (declaring word, iterator, count roles) happens IN THIS LEAF as part of the packaging commit - one file, both concerns coherent here because ownership is what unblocks the conversion. Acceptance: whitespace-edit probe passes the package gate after; the consuming lint suites green; boundary-aware sweep shows zero raw vector references in the file; both diff lints.

DELIVERED IN-LANE (not on master): first as lane commit 6c119985 "Package lint interner as LINT-INTERN", then rejected in review and rebuilt as lane commit a3785217 "Package lint interner with transactional appends", which is the current delivery. The revision deletes the duplicate ENTRY-N / CHUNK-N cardinality counters (column length is the single authority), reserves every column and chunk mapping before any push so publication cannot fail halfway, carries count and index roles end to end behind two audited trusted projections, and adds a six-leg allocation-failure injection seam. It also fixes a latent defect found on the way: after a throw the chunk cursor was parked past the last row, so the next span allocation read a nonexistent row 192K into a 4K mapping and the process died with exit 134. Neither commit is reachable from master; they live only in the vecmem lane workspace .jj-ws/habu-pkg-vecmem.

Claim: agent=vecmem-lane workspace=.jj-ws/habu-pkg-vecmem (delivered in-lane at a3785217, replacing the rejected 6c119985; codex re-review and integration pending)
