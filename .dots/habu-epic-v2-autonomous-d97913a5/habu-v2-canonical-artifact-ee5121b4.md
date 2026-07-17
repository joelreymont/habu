---
title: V2 canonical artifact envelope
status: active
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-11T12:25:26.829932+02:00\\\"\""
---

Implement MODEL-CAD-V2-PLAN.md:1812-1830 first artifact slice. Define checked Artifact<Kind> metadata schemas for schema id/version, kind, payload digest, producer, source revisions, dependencies, target/config/numeric facts, capabilities, and event id using required V2 nominal/layout types; add canonical encode/decode and malformed/noncanonical/kind-mismatch negatives. No raw n identity or trust boundary. Acceptance: equal values encode/hash identically, one semantic field changes the digest, decode round-trips, unknown required fields and digest mismatch return typed diagnostics. Files: new maki/db/artifact.f plus focused test and FILEMAP.

Claim: agent=artimpl workspace=.jj-ws/fable-artimpl (implements the frozen contract habu-freeze-canonical-artifact-3b6b7087; owns maki/db/artifact.f (new) + focused test)

FIRST SLICE LANDED 2026-07-17 (artimpl lane, commit d0754aa6; claim
RELEASED). maki/db/artifact.f + artifact-test.f: wire envelope per the
frozen contract (ascending tagged length-delimited fields, LE widths,
digest-covered ver/kind/producer-ver/id/deps; digest+event excluded;
unknown-required rejected; unknown-optional retained opaquely), checked
types (weight-artifact/kernel-artifact PRODUCT kind separation,
content-digest 4-word PRODUCT, art-result SUMTYPE with the 8 taxonomy
variants - no throws for domain outcomes), SHA-256 reused from baked
src/core/sha256.f, artifact-id via the existing blessed refinements - no
new trust boundary. ALL 12 acceptance tests green, wired into maki/test.f
(111 PASS). REMAINDER blocked on contract round 2 (see
habu-artifact-contract-r2 dot): the producer/source-rev/target/config/
numeric/capability/audit-event fields cannot join the envelope until
their owner families get constructors/wire-codecs, and the envelope
VALIDATE could not be published due to the ARTIFACT:VALIDATE name
collision (digest verification folded into DECODE meanwhile).

ROUND-2 NOTE 2026-07-17: contract gaps resolved (676d5a7b). Remaining for
this dot when the id-family codecs land: add the foreign-id fields to the
envelope via the owner-package codecs, publish the envelope VALIDATE (tail
now free), and reconcile the process-local P-ID dependency wire form with
the cross-process content-key form (flagged in plan 23.9).

Claim: agent=artimpl2 workspace=.jj-ws/fable-artimpl2 (second slice: foreign-id fields via the landed codecs + envelope VALIDATE; owns maki/db/artifact.f)

SLICE 2 LANDED 2026-07-17 (artimpl2 lane, commit 469f1e15; claim RELEASED).
The five expressible foreign-id fields joined the envelope (tags 6-10:
schema/producer/config/npol/target, all digest-covered semantic fields,
serialized across the owner package boundaries - zero new refinements in
ARTIFACT), and the envelope ARTIFACT:VALIDATE landed on the freed tail
(kind-agnostic structural+digest verification sharing DECODE's core, same
art-result taxonomy). Per-field digest-flip + reject coverage; target-id
proven on the DECODE side because the shared 16-slot target registry is
at capacity in integration (documented at the test site + LESSONS).
DOT REMAINDER (slim): capability-id field (waits habu-user-gated-cap-
edccf572 vocabulary), audit-event-id + rev-id/source-revisions fields
(wait the txn/journal dot's JOURNAL + REV/TX codecs), and the
process-local P-ID vs cross-process content-key wire reconciliation
(plan 23.9 flag). TAG-CAP-RESERVED + TAG-EVENT commentary hold the wire
slots.

NOTE 2026-07-17: JOURNAL/REV codecs landed (16de285f) - the envelope's
audit-event-id and rev-id/source-revisions fields are now expressible;
only the user-gated capability field and the wire-form reconciliation
remain gated.

Claim: agent=artimpl3 workspace=.jj-ws/fable-artimpl3 (slice 3: event/rev envelope fields via JOURNAL/REV codecs; owns maki/db/artifact.f)
