---
title: Shorten CHECK nominal diagnostics
status: open
priority: 1
issue-type: task
created-at: "2026-07-23T02:13:02.539233+02:00"
blocks:
  - habu-shorten-check-dep-f84a6d3c
---

Why: the nominal name parser and its diagnostics are one private, byte-exact concern still carrying legacy CHK names. Owner: package CHECK. Files: tools/check-core.f and tools/check-test-lib.f. Rename only the private words and storage from WORD-TOK? through LIN-REGISTER: token-kind and case-insensitive token helpers, token-end/source extraction, JSON and prose nominal diagnostics, type/nominal/linear failures, family-tail folding, reserved-name rejection, and DEFTYPE/DEFLINEAR registration. Use short package-local tails and update only direct callers. Keep diagnostic keys, labels, spans, suggestions, throw codes, registration order, lowercase family-tail derivation, and public CHECK behavior byte-exact. Acceptance: zero executable CHK-prefixed name remains in this concern; DEFTYPE and DEFLINEAR valid, invalid, duplicate, reserved, mixed-case, package-qualified, comment, string, and malformed fixtures execute the production CHECK path and preserve exact prose and JSON. Forbidden: aliases, copied tokenization, normalized diagnostics, public helpers, runtime name heuristics, or changing declaration semantics. Pre-change proof: a representative short nominal helper fails package ownership outside CHECK and passes only as private package state. Verify through nominal and linear tools/check-test.f fixtures, verify-source parity, exact diff ownership/type, and host gates.
