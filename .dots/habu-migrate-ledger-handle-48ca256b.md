---
title: Migrate ledger handle to STRUCTURE
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-24T14:01:55.900337+02:00\""
---

Why: maki/db/budget-ledger.f still declares the public LEDGER:ledger handle through legacy PRODUCT even though the unified STRUCTURE authority generates the same nominal record surface. Exact result: replace only LEDGER:ledger with STRUCTURE, preserving package, public family identity, field name slot, field schema n, one-cell tagless layout, LEDGER-LEDGER:MAKE/UNMAKE spellings and effects, >LEDGER/LEDGER>, pool ownership, range validation, request accounting, digests, errors, and all callers. Retarget every ledger-specific PRODUCT comment; leave budget-result SUMTYPE unchanged. Owner: package LEDGER and maki/db/budget-ledger.f plus focused budget-ledger tests. Dependencies: unified STRUCTURE is landed; no ENUM migration or storage redesign. Forbidden: alias, cast, parser change, wrapper rename, compatibility declaration, result migration, pool/accounting change, or copied test model. Pre-change proof: the Habu source lexer finds exactly one executable PRODUCT declaration in the production file; a representative direct opener/closer replacement must pass package-diff lint and the real budget-ledger suite. Acceptance: exact reflection proves one field named slot at position zero with schema n and width one; generated MAKE/UNMAKE effects and runtime round trip match the parent; OPEN, LIMIT!, RESERVE, CHARGE idempotency, replay ordering, DIGEST, invalid handle, capacity, and buffer failures execute through maki/db/budget-ledger-test.f; executable PRODUCT count becomes zero; focused typed-local, package, trust, and source gates pass. Smallest owning-path check: bin/hb --load maki/db/budget-ledger-test.f.

Claim: agent=codex-ledger-structure workspace=.jj-ws/habu-migrate-ledger-handle-48ca256b
