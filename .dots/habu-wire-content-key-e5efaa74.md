---
title: Wire content-key cross-process forms into id codecs
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-17T18:52:08.302608+02:00\""
---

The plan-23.9 origin-class table specifies cross-process wire forms (32-byte SHA-256 content key for content-addressed families; 8-byte sequence for append families), but every landed codec (artifact P-ID + TAG-ID/TAG-DEPS, target, npol, schema, producer, config, rev - and the rev-id set elements in TAG-SREVS) currently serializes the PROCESS-LOCAL registry raw (8B LE), documented per the explicit out-of-scope note from the contract round. This dot owns the migration: (1) decide the versioning path (envelope schema-version bump vs per-field wire-version) per the 23.9 migration rules; (2) each content-addressed owner package gains KEY>WIRE/WIRE>KEY over its canonical 32-byte content key (the registries already intern by content, so the key exists - expose it per family); (3) the envelope and transaction codecs switch their id fields to the cross-process forms under the versioned migration, with old-version decode either migrated deterministically or rejected unsupported-migration per contract; (4) round-trip + cross-process tests: encode in one process image, decode in a fresh one (spawn a child bin/hb - the grader spawn pattern), proving identity survives; (5) JOURNAL/audit-event stays sequence-keyed (occurrence identity - confirm the cross-process story for sequences: journal replica identity is a design point, flag if the plan is silent). Constraints: the digest-coverage table is frozen - the wire migration must not change WHICH fields are covered, only their encoding; all existing suite acceptance stays green under the new version; error codes grep-verified + error-code-lint. Files: maki/db/artifact.f, maki/db/transaction.f, the seven owner packages, tests. Ownership: V2 artifact wire migration.

Claim: agent=keywire workspace=.jj-ws/fable-keywire (owns maki/db/artifact.f + transaction.f + the seven owner packages for the wire migration)
