---
title: Mint and erase a DEFLINEAR value from checked code
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T20:26:05.725562+03:00"
---

Problem: a DEFLINEAR type has no checked producer or consumer: CAST: refuses a linear source or destination (E-CAST-LINEAR, 7137) and a colon body cannot produce one, so every shipped linear resource (lib/json-read.f, lib/byte-edit.f, lib/xml/state.f, lib/process-pty-handle.f) mints and erases its token through a TRUSTED: trio, and a library forbidden TRUSTED: (lib/db/pq.f, 2026-09-16) had to replace linear ownership with a runtime slot registry. Acceptance: the checker offers a package-private mint and erase for a DEFLINEAR type declared in that package (the owner-private row mechanism of PPRIM:/CLOSE-PRIVATE, or a LINEAR-MINT:/LINEAR-ERASE: pair the type's defining package alone may call), so the owning module produces and consumes its token in checked code and no other scope can; the four shipped linear modules converted one file per commit with their TRUSTED: trios deleted; lib/db/pq.f's result owner moved to it in a follow-up dot. Files: src/core/checker.f, lib/type/deflinear.f (or where DEFLINEAR lives), the four modules, tests in test/cast-negative-suite.f and lib/type tests. Verify: the linear suites and test/run.f green on a rebuilt engine; rg -c '^TRUSTED:' on the four modules drops by three each. Depends: habu-honour-owner-private-0a19f45d. Ownership: checker and lib/type. Claim: unassigned.

## Design (2026-10-01, Fable plan; full text ~/.cache/tmp/heron-arm64/design-linear-mint.md)

Decision: one engine reader keyword `LINEAR:` (both directions, like `CAST:`), certified by a new checker registrar CHECKER-LINEAR through a new DECLARATIONS slot LINEAR-OFF. The DEFLINEAR type table records its declaring package (CT owner columns, persisted with the table), so the checker can admit a `LINEAR:` row only in that package's private section. Rejected: PPRIM:/CLOSE-PRIVATE (an axiom about an engine primitive with an unchecked body, against the rule that PRIM axioms are only engine, syscall and FFI boundaries); relaxing CAST: (linear sides stay E-CAST-LINEAR 7137, in the owner too); a LINEAR-MINT:/LINEAR-ERASE: pair (the signature already gives the direction).

Rules: exactly one side is a linear con and the other a non-linear payload (a con or a pointer to one), one cell each, else E-LINEAR-PAYLOAD 7148; the linear type's declaring package is the current package, else E-LINEAR-OWNER 7149 (an unowned top-level DEFLINEAR has no mint); the row is in that package's private section, else E-LINEAR-SCOPE 7150. Arity and unknown types reuse E-CAST-ARITY and E-CAST-FAM.

Order: after B11 (habu-honour-owner-private-0a19f45d): the verify-source pre-pass must call CHECKER-LINEAR from checked code through a private-only VERIFY row, which only B11 makes callable. Then one checker commit (worker-max: checker.f, checker-owner-abi.f, layout.f, habu2.f, verify-source.f, tools/lint/def.f, new test/linear-suite.f registered after SUITE cast, docs), then one commit per module: lib/byte-edit.f, lib/xml/source.f, lib/xml/state.f, lib/json-read.f (INIT takes `ptr n` storage, retiring STORAGE>PREMINT), lib/process-pty-handle.f (its DEFLINEARs move inside package PROCESS-PTY). Codes 7148-7150 sit after B10a's E-CAST-MINT 7147.
