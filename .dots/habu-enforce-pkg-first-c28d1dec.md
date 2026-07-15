---
title: Enforce package-first Forth modules
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-15T21:54:27.612398+02:00\""
---

Full context: docs/forth.md:91-123 requires real package namespaces and forbids raw global prefix stems, but tools/namespace-lint-core.f only enforces maki and exempts tests, so generated tools/tests/libs can still introduce LRD-MISSING$/TST-* globals. Fix: add a checked Habu diff gate that rejects new or changed definitions outside package scope in lib/, tools/, test support, maki/, and src subsystem files, with only explicit documented core/prelude and cross-cutting error-code exceptions; recognize all project definers case-insensitively; fail closed on malformed package scopes; require package-local short private names and public NAME:WORD APIs by construction; integrate the gate into the native commit/full-gate path; migrate every new word touched by this goal rather than allowlisting stems. Acceptance: fixtures reject raw-prefix and unprefixed global defs, upper/lowercase definers, package underflow/unclosed scope, and changed-definition evasion; valid private/public/reopened packages and documented core/error exceptions pass; live diff is clean; typed-local, focused lint tests, host/filemap/dot, maki, ptx-stdlib, fixpoint, and full native gates pass. Files: new package-diff lint core/runner/tests plus owning gate manifest and FILEMAP after active owners release them; docs/forth.md rule is already normative.
