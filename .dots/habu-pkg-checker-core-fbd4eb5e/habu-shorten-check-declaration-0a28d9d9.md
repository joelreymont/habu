---
title: Shorten CHECK declaration scan
status: open
priority: 1
issue-type: task
created-at: "2026-07-23T02:13:12.815522+02:00"
blocks:
  - habu-shorten-check-nominal-c62d19fb
---

Why: nominal block declarations and package-state scanning form one private parser concern after nominal diagnostics are package-owned. Owner: package CHECK. Files: tools/check-core.f and tools/check-test-lib.f. Rename only the private words and storage from VREC-FAIL through NOM-STEP: record buffer and registration, TYPEFAMILY, SUMTYPE, ENUM, PRODUCT registration, declaration packet capture/flush/failure, shared block collection, package visibility steps, definition-opener recognition, definition skipping, and nominal scan dispatch. Use short package-local tails and update only direct callers. Preserve declaration token consumption, unterminated-block behavior, arity diagnostics, package visibility, source spans, packet shape, registration order, and verify-source parity exactly. Acceptance: zero executable CHK-prefixed name remains in this concern; valid and hostile record, type-family, sum, enum, product, package, public/private, nested, missing-name, missing-arity, unterminated, comment, string, duplicate, wrong-role, and qualified-use fixtures execute the real CHECK path with byte-exact prose and JSON. Forbidden: aliases, second declaration parser, substring matching, eager package mutation outside the checker scope, public helpers/state, or behavior changes. Pre-change proof: a representative short declaration helper fails package ownership outside CHECK and passes only as CHECK-private. Verify through declaration and package tools/check-test.f fixtures, verify-source parity, exact diff ownership/type, host, file-map, and gate dictionary.
