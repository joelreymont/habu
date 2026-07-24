---
title: Migrate diagnostic handle to STRUCTURE
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-24T14:02:05.193400+02:00\""
---

Why: maki/db/diagnostic.f still declares the public DIAG:diagnostic handle through legacy PRODUCT even though unified STRUCTURE owns nominal records. Exact result: replace only DIAG:diagnostic with STRUCTURE, preserving package, public family identity, field name slot, schema n, one-cell tagless layout, DIAG-DIAGNOSTIC:MAKE/UNMAKE effects, >DIAG/DIAG>, pool ownership, validation, wire codec, errors, and all callers. Retarget every diagnostic-specific PRODUCT comment; leave class/severity/phase/repair ENUMs and build-result/decode-result SUMTYPEs unchanged. Owner: package DIAG and maki/db/diagnostic.f plus focused diagnostic tests. Dependencies: unified STRUCTURE is landed; no ENUM migration or diagnostic redesign. Forbidden: alias, cast, parser change, wrapper rename, compatibility declaration, result migration, pool/codec change, or copied validator. Pre-change proof: the Habu source lexer finds exactly one executable PRODUCT declaration in the production file; a representative direct replacement must pass package-diff lint and the real diagnostic suite. Acceptance: parent/candidate reflection proves field slot at position zero with schema n, width one, identical generated effects and runtime round trip; the real builder, missing-owner/reproduction refusals, encode/decode, malformed/noncanonical/bounds/duplicate/unknown-required cases, validation, ordinal bridges, and lookup path pass through maki/db/diagnostic-test.f; executable PRODUCT count becomes zero; focused typed-local, package, trust, and source gates pass. Smallest owning-path check: bin/hb --load maki/db/diagnostic-test.f.

Claim: agent=codex-diagnostic-structure workspace=.jj-ws/habu-migrate-diagnostic-handle-4f056da6
