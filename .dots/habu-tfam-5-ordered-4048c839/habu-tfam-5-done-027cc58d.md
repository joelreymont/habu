---
title: "TFAM 5 done: shared path emitter + loader reservation"
status: closed
priority: 3
issue-type: task
created-at: "\"2026-07-04T08:53:29.654234+02:00\""
closed-at: "2026-07-04T08:54:20.773412+02:00"
close-reason: landed as commits lyxrxwqv + qlmumzns in workspace tfam-5
---

COMPLETED in workspace tfam-5 (commits lyxrxwqv 'shared checked path emitter, fail-closed' + qlmumzns 'reserve loader words in reserved-name-lint'). Slice A: SOURCE-APPEND-QPATH/SOURCE-PATH-SAFE?/SOURCE-QPATH-CHECK in lib/source.f + E-FS-PATH-UNSAFE (-2107) in lib/errors.f; rejects paths with quote(34)/backslash(92)/LF(10)/CR(13) fail-closed; wired into SOURCE-APPEND-PROVIDED (byte-identical for safe paths), tools/check-core.f CHK-APPEND-REQUIRED and CHK-BUILD-PREFIX DIAG-FILE! label. Tests in lib/source-test.f (TEST-QPATH-SAFE/SPACE/DQ/BS/LF/CR). Slice B: RNL-RESERVED-LOADER? in tools/reserved-name-lint-core.f reserves include/included/require/required/provided (E-RESERVED-DEFINITION); tests in tools/reserved-name-lint-test-lib.f RNLT-TEST-LOADER. Gate-safe: reserved-name-lint only runs inside tools/check.f over user source, never src/core/include.f. These are prerequisites; do NOT redo them.
