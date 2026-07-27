---
title: Package public-signatures core module
status: open
priority: 2
issue-type: task
created-at: "\"2026-07-27T08:47:43.798709+02:00\""
blocks:
  - habu-own-nominal-linear-491d11e4
---

Seventh wall instance (intern checkpoint): tools/public-signatures-core.f is unpackaged, so the intern migration's edit to PS-EXPORTED? trips E-PACKAGE-OWNERSHIP. NOT contained: 198 definitions, 5 consumers including gate infrastructure (test/gate-diagnostics.f, test/gate-runner-support.f, two run-worker-diag files). MEASURE-FIRST checkpoint mandatory: public surface, consumer reference counts by resolved owner, packaged-status of every file to be edited (the standing lesson: consumers being identifiable is not consumers being editable), and the load-position audit for gate-loaded files - report before any edit; STOP if the cascade escapes tools/ and test/ gate files. Then: one package, short tails, consumers migrated same-commit or split at proven-green seams. Acceptance: whitespace probe passes; the five consumer suites and the gate slices that load them green; both diff lints.

MEASURE-FIRST DELIVERED (2026-07-27, vecmem2; the dot is now dispatch-ready as an explicitly-allowed-large single-commit leaf). Census: 223 definitions, not 198 (143 colon, 46 variable, 25 constant, 9 create; every one PS-prefixed, no package). Cross-file surface is 15 words. Exactly three files need source edits: tools/public-signatures.f (whole body is the single token PS-MAIN - defines nothing, so ownership lint cannot fire; one qualified token), tools/public-signatures-test.f (requires the core at line 19, calls 14 of the 15 words, itself unpackaged so it needs its own package in the same commit), test/gate-diagnostics-lib.f (already package GATE-DIAGNOSTICS; nine call sites near lines 805-809 get qualified). Eight further consumers load it but never name a word - pure verification obligations, no edits (gate-diagnostics.f:22, gate-runner-support.f:45, both run-worker-diag files, gate-stdlib-inline-lib.f:251/:305, gate-stdlib-cases.f:181, standalone-load-test.f:55, stdlib-manifest-test.f:706-721). Collision oracle over all 223 stripped tails: 12 collisions (C! C@ CR DIE DIGIT? LAYOUT-BUFFER LOWER? TK-ENUM TK-PRODUCT TRUST UPPER? WRITE); reserved among tails: I, LAYOUT-BUFFER, TRUST. PS-TRUST is ON the public surface and collides, so its public tail must be probed fresh (TRUST? or TRUST-MODE candidates). Mechanism: one package PUBLIC-SIGNATURES, whole body private, 15 EXPORT lines, three consumers same-commit - MUST be one commit because rename and qualification cannot straddle the gate library. Known traps, in order: (1) stdlib-manifest-test.f:706-721 spawns a child bin/hb that loads the core plus the CLI whose whole body is PS-MAIN - if that token is not migrated the failure surfaces as a manifest-content mismatch far from the cause; (2) run tools/standalone-load-test.f:55 FIRST after the edit - it is the exact guard against a silently missing package opener; (3) gate-diagnostics-lib.f compiles PS-SCAN-FILE inside quotations at load time - load order holds today, any reordering is E-UNDEFINED at gate load. Owning gates: gate-diagnostics, gate-stdlib lint-tools + diagnostics slices, standalone-load-test, stdlib-manifest-test, public-signatures-test, host-lint, filemap-lint.

DELIVERED IN-LANE (not on master): lane commit 97642e1c "Package public-signatures core and its callers" creates package PUBLIC-SIGNATURES with a thirteen-word export surface (MAIN stays private and the command-line entry reopens the package), puts the suite in its own package, and produces byte-identical outputs (18723-byte JSON, 133-line trust report). Measured divergences from the plan above, all confirmed and none scope-expanding: 224 definitions rather than 223, and the real cross-file surface is 13 words rather than 15 because two of the counted names were a comment and a fixture string, resolved by call rather than by token presence. That commit is not reachable from master; it lives only in the vecmem lane workspace .jj-ws/habu-pkg-vecmem.

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
