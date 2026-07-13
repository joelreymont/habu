---
title: Freeze canonical artifact type and wire contract
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T16:15:58.437032+02:00"
blocks:
  - habu-v2-types-artifact-6ee556f8
---

Problem: MODEL-CAD-V2-PLAN.md:2550-2568 describes the canonical artifact envelope conceptually, but implementation would currently invent authority-bearing semantics: CAD-KIND lacks artifact-kind, producer, config, numeric-policy, capability, and persistent audit-event identities; owned canonical byte/vector representations are not frozen; and field order, digest coverage, compatibility, and typed failure behavior are unspecified. Fix: reserve package ARTIFACT for the canonical envelope while keeping ART for typestate; define Artifact<Kind> versus CAD-KIND:artifact-id/content digest/ART:built relations; define owned bytes/vectors and a persistent audit-event id distinct from runtime async events; freeze tags/order/endianness, duplicate and unknown-field policy, schema migration rules, and exact digest inclusion/exclusion; specify checked ENCODE, DECODE, DIGEST, and VALIDATE signatures returning typed diagnostics with no new trust boundary. Acceptance: MODEL-CAD-V2-PLAN.md and type-family docs contain the complete package/wire contract, cross-kind and event-identity confusions are statically untypeable, malformed/noncanonical/digest-mismatch outcomes are explicit typed results, and the implementation dot can proceed without choosing semantics. Files: MODEL-CAD-V2-PLAN.md, docs/type-families.md, maki/cad-kinds.f and focused design fixtures only if needed. Verify: CHECK! positive/negative fixtures, CAD-kind focused test, maki/test.f, typed-local-diff lint if Forth changes.
