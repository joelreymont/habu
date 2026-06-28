---
title: Spanful signature parser
status: open
priority: 2
issue-type: task
created-at: "2026-06-28T19:00:24.990216+02:00"
---

Problem: diagnostics contract exists, but src/core/checker.f signature parsing still has catch-all paths and does not carry token spans through every type-syntax node. This makes future syntax such as nominal declarations and structured TRUST adapters likely to regress to generic bad-signature errors. Fix: promote the signature parser to a recursive-descent parser with source span per token/node and structured error records. Acceptance: every signature parse error emitted by tools/check.f --json-errors has code, line/col/byte span, repair_class, suggestion, and offending token; add fixtures for unknown token, bare ptr, malformed quotation, bad parametric type, bad nominal declaration; diagnostics gate enforces the contract.
