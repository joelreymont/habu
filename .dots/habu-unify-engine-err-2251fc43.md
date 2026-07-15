---
title: Unify engine error namespace
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-15T03:02:16.293300+02:00\""
---

Full context: checker-current-wid assigns exit 86 to E-CALLABLE-ABI while catch-return independently assigns 86 to E-CATCH-STACK and 87 to E-CODE-CERT. Before integration, define one authoritative package-based ENGINE-ERROR ABI in src/habu/layout.f with recovery mirror, preserve existing 83-85 codes, allocate unique CALLABLE-ABI/CATCH-STACK/CODE-CERT codes, remove new global E-* aliases, and add native/recovery uniqueness and exact-exit tests. This is an integration dependency; do not patch lanes independently.

Implementation state: the authoritative ABI is an early sealed `ENGINE-ERROR`
package in `src/core/engine-error.f`, loaded before the checker and layout so
every compiler source consumes one definition. `engine-error-effects.f` reopens
the package after checker startup solely to register exact immutable effects.
Native and recovery package bridges permit a missing checker only while the
friend latch is open, then fail closed. Values are 83 through 88 in the order
specified above; all global `E-*` aliases are absent. Full no-binary bootstrap,
fixpoint, package/seal, engine, boot-pin, trust/inventory, typed-local, host,
filemap, and error-code focused gates pass on this lane.

Fresh destruction fixes add `ENGINE-ERROR` to the checker's case-insensitive
sealed set, checker-only export/layout negatives, and one package-local Habu
behavioral test. The test executes 86/87/88 in child processes, proves a normal
post-seal package reaches the checker bridge, then patches the candidate's sole
embedded checker-package lookup token and proves fail-closed rc 70. The audited
bootstrap runs the same test against its Gforth-recovered candidate.

Exact-tree proof after those fixes: no-binary bootstrap and recovered behavioral
test pass; native `test/run.f` passes in 75,277 ms under the 76,300 ms calibrated
scratch budget; typed-local, trust, trusted-inventory, host, filemap,
suite-coverage, bootstrap-codegen, engine, export, package, seal, boot-pin,
fixpoint, and error-code gates pass.
