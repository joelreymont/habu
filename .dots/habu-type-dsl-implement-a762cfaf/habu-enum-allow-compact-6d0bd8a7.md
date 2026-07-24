---
title: "ENUM: allow compact headers"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-24T23:00:45.771927+02:00\""
blocks:
  - habu-enum-parse-full-39c0dc1b
---

Why: src/core/enum-decl.f selects compact mode for payloadless ENUM declarations but rejects POLICY and DERIVE there, even though these header clauses are independent of payload shape and the existing public ENUM surface uses them broadly. Forcing equivalent payloadless declarations into full VARIANT blocks would make the unified grammar needlessly verbose and turn the hard cutover into hand migration. Exact behavior: compact mode remains arity zero and TK-ENUM. It accepts optional POLICY and DERIVE header clauses before the first variant, using the existing header/event owners and validation. Once the first variant is consumed, every header token rejects; compact variants remain payloadless and reserved header words remain invalid variant names. Full mode and mixed-mode rejection stay unchanged. Owner: package ENUM-DECL; no legacy parser change, compatibility alias, kind flip, reflection widening, second derive parser, or constructor work. Production proof: extend test/enum-decl-suite.f through ENUM-DECL:ED-RUN with no-header, policy, derive eq/hash in both orders, combined headers, duplicate headers/features, missing values, header-after-variant, reserved-name, rollback, kind, event, and family metadata cases. Mutations restoring COMPACT-KW? rejection, admitting a late header, or registering TK-SUM must fail. Files: src/core/enum-decl.f, test/enum-decl-suite.f, grammar documentation only where the compact shorthand is specified. Verify: enum declaration, declaration-event, type-family, candidate validation, typed-local, package, trust, and fixpoint gates. Depends: habu-enum-parse-full-39c0dc1b. Claim: agent=enum_compact_headers workspace=.jj-ws/habu-enum-allow-compact-6d0bd8a7.
