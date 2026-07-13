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

## Frozen design slice (2026-07-13)

Add nominal `CAD-KIND:artifact-kind`, `producer-id`, `config-id`,
`numeric-policy-id`, `capability-id`, and persistent `audit-event-id` in
`maki/cad-kinds.f`; do not model the 256-bit content digest as a one-cell kind.
`ARTIFACT` owns the canonical envelope, while `ART` remains typestate and
`ART:built` names a validated `CAD-KIND:artifact-id`. CUDA and future
`ASYNC:event` values remain ephemeral synchronization resources and cannot
unify with audit provenance.

Freeze conceptual checked signatures for `ARTIFACT:ENCODE`, `DECODE`, `DIGEST`,
and `VALIDATE` over `artifact<k>`, owned canonical bytes, `content-digest`, and
typed `result<...,diag-set>`. The wire format uses fixed little-endian widths,
versioned length-delimited fields, ascending tags, duplicate rejection, unknown
required-field rejection, opaque retention of accepted optional fields,
canonical ordered dependency/capability sets, and exact version or a registered
deterministic migration. The content digest excludes itself, storage location,
timestamps, runtime handles, and audit-event identity; stored-envelope identity
includes canonical provenance and the persistent audit-event link.

Pin verdict-0 negatives with resolving positives for adjacent nominal ids,
`audit-event-id` versus runtime event, `artifact-id` versus content digest, and
`artifact<a>` versus `artifact<b>`. Specify typed malformed, noncanonical,
bounds, duplicate, unknown-required, kind/schema mismatch, unsupported
migration, and digest-mismatch outcomes. This dot owns only the plan/type
contract plus CAD kinds/tests; the encoder remains with the parent implementation
dot. Serialize dispatch until `docs/type-families.md` has exclusive ownership,
then use `dot on` and commit/push the claim before creating its workspace.
