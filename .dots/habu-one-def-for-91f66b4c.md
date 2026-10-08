---
title: One definition for the engine constants the checker reads
status: open
priority: 2
issue-type: task
created-at: "2026-10-08T10:58:28.859923+02:00"
---

Problem: src/core/checker.f keeps its own copies of 33 engine constants because it loads before src/habu/layout.f (comment at checker.f:1060), e.g. `$78 constant CK-PKG-PUB-OFF` = layout.f:487 `$78 constant PKG-PUB-CELL`, CK-DREC = DREC, CK-DICT-CAP = DICT-CAP; test/aot-sig-pool-suite.f MIRROR-CASE exists only to check the copies still match. The checker reads engine memory raw through them (about 41 DATA@/data-base sites).
Acceptance: the engine-layout constants the checker reads (the src/habu/layout.f DATA offsets and record format) load before src/core/checker.f, so the checker names them directly; the CK-* copies and MIRROR-CASE are deleted. Why the checker loads first today (tools/bootstrap.sh:336-352): `0 set-check`, the checker core (SRC_CORE) compiled unchecked, `LOWER-CERT-HOOK:INSTALL`, then SRC_COMMON, layout.f included, compiled checked; anything moved ahead of the checker loses its recorded types unless the definition is the source of its type (blocking dot). Also measure whether the checker walks the using list and package record to resolve names itself where the engine's own lookup would answer; if it duplicates lookup, record that as its own dot.
Files: src/core/checker.f, src/habu/layout.f, src/core/layout-buffer.f, src/core/layout-valid.f, test/aot-sig-pool-suite.f.
Verify: full suite on the built hb.
Depends: none. Ownership: unassigned. Claim: unassigned.

Ruling (Joel, 2026-10-08, supersedes the above where they differ): the compiler core is a small untyped kernel, implicitly trusted and not type-checked; it lives in the system package and is stripped from the delivered hb. No census, no certification ceremony; the kernel is covered by integration tests (external proofs only if ever needed).
Consequence: the kernel is unchecked, so loading the layout constants ahead of the checker needs no recorded types; this dot no longer waits on the type-from-definition dot.
