---
title: "SCHEMA: schema-id registry + wire codec"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-17T15:33:59.945354+02:00\""
---

Per-family leg of plan 23.9 foreign-id contract (676d5a7b): new owner package SCHEMA with a content-addressed registry intern origin; NEEDS-DECISION (engineering, resolve in-dot): the schema-definition grammar that gets interned. Retire the RAW>SCHEMA-ID placeholder in maki/evidence/policy.f as part of landing. Publish constructor + refinements + SCHEMA:ID>WIRE / WIRE>ID (32-byte content-key class) with tests. Files: new maki schema owner file, maki/evidence/policy.f placeholder retirement, focused test, FILEMAP. Ownership: V2 artifact id codecs.

Claim: agent=idfam2 workspace=.jj-ws/fable-idfam2 (tri-dot lane; disjoint new owner files)
