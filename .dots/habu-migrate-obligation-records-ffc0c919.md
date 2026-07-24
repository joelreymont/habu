---
title: Migrate obligation records to STRUCTURE
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-24T14:08:44.719591+02:00\""
---

Why: maki/db/obligation.f still declares public OBL:obligation and OBL:evidence through legacy PRODUCT even though unified STRUCTURE owns nominal records. Exact result: replace only both declarations with STRUCTURE, preserving packages, public identities, obligation slot:n and evidence slot:n fields, one-cell tagless layouts, generated MAKE/UNMAKE spellings and effects, internal wrappers, pool ownership, canonical identities, codecs, validation, discharge semantics, errors, and every caller. Retarget obligation/evidence-specific PRODUCT comments; leave discharge-result/decode-result/id-result SUMTYPEs unchanged. Owner: package OBL and maki/db/obligation.f plus focused obligation tests. Dependencies: unified STRUCTURE is landed; no ENUM, result, codec, store, or policy redesign. Forbidden: alias, cast, parser change, wrapper rename, compatibility declaration, SUMTYPE edit, id/wire change, proof exposure, or copied model. Pre-change proof: the Habu source lexer finds exactly two executable PRODUCT declarations in the production file; a representative direct replacement must pass package-diff lint and the real obligation suite. Acceptance: parent/candidate reflection proves both slot fields at position zero with schema n, width one, identical generated effects and round trips; production intern/lookup, evidence identity, canonical codec, discharge, mismatches, malformed/noncanonical/bounds/duplicate/unknown-required, capacity, and validation paths pass through maki/db/obligation-test.f; executable PRODUCT count becomes zero; focused typed-local, package, trust, and source gates pass. Smallest owning-path check: bin/hb --load maki/db/obligation-test.f.

Claim: agent=codex-obligation-structure workspace=.jj-ws/habu-migrate-obligation-records-ffc0c919
