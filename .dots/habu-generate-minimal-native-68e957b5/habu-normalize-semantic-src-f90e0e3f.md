---
title: Normalize semantic source layout
status: open
priority: 3
issue-type: task
created-at: "2026-07-21T22:41:12.570890+02:00"
---

Invariant: physical layout exposes semantic units rather than compressing them onto one line. A multiline signature places its body or declaration clauses on following lines; repeated builder calls use one logical operation per line; long construction is factored or vertically grouped at typed seams; an empty SUITE clause is one line. This is syntax-driven, not an arbitrary column limit. The current tree has confirmed dense kernel declarations, tensor and shape constructions, fusion builders, and 378 two-line empty suite clauses; raw long-line counts also include valid byte goldens, URLs, tables, and generated subjects that must remain untouched.

Write precise source-layout rules in the Forth standard and build checked tokenizer-aware lints only for structurally provable cases: body tokens after a multiline signature, multiple repeated builder terminators on one line, and split empty clauses. Audit the long-line census manually for additional semantic-density defects, factor code only at real typed responsibility seams, and preserve unavoidable literals and data. Coordinate canonical suite generation with its existing owner so generated empty clauses use the normalized form.

Add positives and negatives for KERNEL and other declarers, nested generic signatures, one logical builder operation, vertically grouped arguments, empty and argument-bearing suites, strings, comments, exact goldens, URLs, and generated checked fixtures. Verify every touched exact load, emitted PTX and graph bytes, suite inventories and order, formatting and typed-local lints, bootstrap, fixpoint, Maki, PTX standard library, and full native gates. Measure source tokens and lines, definitions, JIT, DATA, CODELEN, and compile time; pure layout changes must be runtime-byte neutral and factoring must show a measured improvement or unchanged code.
