---
title: Cut ENUM to binder FIELD grammar
status: active
priority: 1
issue-type: task
created-at: "2026-07-30T23:53:23.711587+02:00"
---

Problem: package ENUM-DECL in src/core/enum-decl.f still requires a naked decimal arity to select full mode and maps one-letter payload parameters by alphabet position. The hard-cut surface is ENUM name<binders>; binder order defines schema ordinals; full payloads are written only as FIELD name type inside VARIANT blocks. Both live and replay callers already converge on ED-GUARDED and DRIVE.

Result: in DRIVE, parse the family token through the existing DECL-HEAD:PARSE before REQUIRE-NAME, register the stripped tail, and use the explicit-head flag to select full mode. An explicit head, including name<>, selects full TK-SUM mode; a bare head retains compact payloadless TK-ENUM mode. In LETTER-TYPE, resolve concrete n, f, and r first, then resolve only a declared binder with DECL-HEAD:PARAM?; a valid but undeclared parameter rejects E-PAYLOAD. Delete ED-ARITY, ED-SI, ED-DIGIT?, ED-ALLDIG?, DEC, ARITY-WHY$, PARSE-ARITY, and their obsolete registration stores. Rename package-local E-ARITY to E-HEAD. Preserve FIELD-CLAUSE, VARIANT-CLAUSE, FULL-BODY, COMPACT-BODY, declaration transactions, constructor generation, compact headers, and replay transport unchanged. Do not modify DECL-HEAD or any caller outside ENUM-DECL and its two owning suites.

Production defect: ENUM-DECL:DRIVE rejects name<e,a> before head parsing, chooses full mode from a naked decimal body token, and assigns e/a by alphabet position rather than declared order. Acceptance fixtures in test/enum-decl-suite.f and test/decl-replay-verify-source.f author the final grammar and first execute in M17: pair<e,a> records arity 2 with e ordinal 0 and a ordinal 1; a phantom binder records arity 1; name<> is full TK-SUM; bare compact ENUM remains TK-ENUM; a valid but undeclared binder rejects E-PAYLOAD with byte-identical rollback; duplicate or malformed heads reject E-HEAD; retired ENUM name 1 syntax rejects and rolls back; bare positional VARIANT payload rejects while FIELD value a succeeds; live and replay declarations compare equal while replay retains its no-constructor-symbol distinction.

Files: src/core/enum-decl.f, test/enum-decl-suite.f, test/decl-replay-verify-source.f. Owner: ENUM-DECL binder-head consumption and final FIELD-only payload grammar. Dependency: the landed DECL-HEAD interface from habu-parse-structure-binder-1da50c1c. Before M17, acceptance is exact hunk review, final-spelling census, rooted package diff, and typed-local diff only. Forbidden: compatibility grammar, dual parser, numeric arity, alphabet-derived ordinals, bare positional payloads, new parser/type/package/transaction, persistent binder metadata, caller rewrites, constructor changes, compact-mode changes, trust, wrappers, or new gates.

Claim: agent=enum_cut workspace=.jj-ws/habu-cut-enum-to-b83a478f.
