---
title: Package intern lint module
status: open
priority: 2
issue-type: task
created-at: "2026-07-27T00:09:32.400720+02:00"
blocks:
  - habu-pkg-set-test-07723f0e
  - habu-pkg-err-code-9508f911
  - habu-pkg-repl-lint-cd31219c
  - habu-pkg-public-signatures-e25db8b1
  - habu-extend-typed-vector-320e1620
  - habu-own-nominal-linear-491d11e4
---

Fifth unpackaged-legacy wall instance, measured: tools/lint/intern.f (191 lines, no package keyword) reds E-PACKAGE-OWNERSHIP on a whitespace-only body edit (intern.f:72 INTERN#), so its 8 raw vector references cannot convert until the file is owned. NOT file-local (checkpoint correction): packaging renames the external surface, and the exact-token consumer set resolved by owner is 7 files, 80 references (set-test 42, error-code-lint-core 12, dot-dep-lint-core 7, filemap-lint 7, repl-lint-core 6, public-signatures-core 3, suite-coverage-lint-core 3) - a naive sweep says 97 across 12, but 17 references in 5 maki/db files are the obligation domain OWN word INTERN (obligation.f:824), a name collision, not consumers: qualify by resolved owner, never by text. ONE commit covering the file plus all seven consumers (split leaves red intermediates; master-always-green forbids it); gate by loading each consumer suite for real; the file is already partly migrated (uses VEC:@, defines its own INTERN>INDEX bridge) and carries a documented residual identical to sched-key's, so its vector-call conversion onto the typed surface (declaring word, iterator, count roles) happens IN THIS LEAF as part of the packaging commit - one file, both concerns coherent here because ownership is what unblocks the conversion. Acceptance: whitespace-edit probe passes the package gate after; the consuming lint suites green; boundary-aware sweep shows zero raw vector references in the file; both diff lints.

DELIVERED IN-LANE (not on master): first as lane commit 6c119985 "Package lint interner as LINT-INTERN", then rejected in review and rebuilt as lane commit a3785217 "Package lint interner with transactional appends", which is the current delivery. The revision deletes the duplicate ENTRY-N / CHUNK-N cardinality counters (column length is the single authority), reserves every column and chunk mapping before any push so publication cannot fail halfway, carries count and index roles end to end behind two audited trusted projections, and adds a six-leg allocation-failure injection seam. It also fixes a latent defect found on the way: after a throw the chunk cursor was parked past the last row, so the next span allocation read a nonexistent row 192K into a 4K mapping and the process died with exit 134. Neither commit is reachable from master; they live only in the vecmem lane workspace .jj-ws/habu-pkg-vecmem.

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

GROOMED 2026-08-04 (dot-groom). The blocker edge to habu-pkg-filemap-lint-5d7baf5c is
removed and that dot is closed: commit 85a9646fd "Delete FILEMAP and census gates"
deleted tools/filemap-lint.f, tools/filemap-lint-test.f and FILEMAP.md, so there is no
filemap-lint left to package. Two consumer counts in the paragraph above are now stale
for the same reason: filemap-lint (7 references) and suite-coverage-lint-core (3
references, tools/suite-coverage-lint-core.f also deleted) are gone, so the resolved
consumer set is 5 files and 70 references, not 7 files and 80. The rest of the contract
is unchanged and still live.
