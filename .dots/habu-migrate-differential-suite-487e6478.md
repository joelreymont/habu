---
title: Migrate differential suite handle
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-24T14:08:34.308671+02:00\""
---

Why: maki/db/diff-suite.f still declares public DIFFSUITE:suite through legacy PRODUCT even though unified STRUCTURE owns nominal records. Exact result: replace only DIFFSUITE:suite with STRUCTURE, preserving package, public family identity, field slot:n, one-cell tagless layout, generated DIFFSUITE-SUITE:MAKE/UNMAKE spellings and effects, internal wrappers, pool ownership, spec builder, digest identity, budget/target/runner bindings, codec, errors, and callers. Retarget every suite-specific PRODUCT comment; leave build-result/decode-result SUMTYPEs unchanged. Owner: package DIFFSUITE and maki/db/diff-suite.f plus focused diff-suite tests. Dependencies: unified STRUCTURE is landed; no ENUM, result, provider, codec, or storage redesign. Forbidden: alias, cast, parser change, wrapper rename, compatibility declaration, SUMTYPE edit, hash/wire change, or copied suite model. Pre-change proof: the Habu source lexer finds one executable PRODUCT in the production file; a direct representative replacement must pass package-diff lint and the real diff-suite suite. Acceptance: parent/candidate reflection proves slot at position zero with schema n and width one, identical generated effects and round trip; production build, reset, digest, order-independence, budget, runner, target, encode/decode, malformed/noncanonical/bounds/duplicate/unknown-required, capacity, and rebuild cases execute through maki/db/diff-suite-test.f; executable PRODUCT count becomes zero; focused typed-local, package, trust, and source gates pass. Smallest owning-path check: bin/hb --load maki/db/diff-suite-test.f.

Claim: agent=codex-diff-suite-structure workspace=.jj-ws/habu-migrate-differential-suite-487e6478
