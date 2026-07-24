---
title: Migrate promotion witnesses to STRUCTURE
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-24T14:08:56.047898+02:00\""
---

Why: maki/db/promotion.f still declares the five public typestate witnesses PROMO:candidate, verified, measured, satisfied, and promoted through legacy PRODUCT even though unified STRUCTURE owns nominal records. Exact result: replace only those five declarations with STRUCTURE. Preserve package, public identities, exact field names/schemas/order, tagless widths, generated MAKE/UNMAKE spellings and effects, private proof-token producers, artifact and revision identity threading, promotion transitions, evidence binding, errors, and callers. Retarget every witness-specific PRODUCT comment; leave all other declarations and policy behavior unchanged. Owner: package PROMO and maki/db/promotion.f plus focused promotion tests. Dependencies: unified STRUCTURE is landed; no ENUM, evidence, policy, store, or transition redesign. Forbidden: alias, cast, parser change, compatibility declaration, proof mint exposure, field/transition change, duplicated state machine, or copied test model. Pre-change proof: the Habu source lexer finds exactly five executable PRODUCT declarations in the production file; a representative direct replacement must pass package-diff lint and the real promotion suite. Acceptance: parent/candidate reflection proves every field name/schema/position, family width, and generated effect identical; real candidate-to-promoted production transitions, artifact/revision identity, evidence applicability, private proof rejection, wrong-stage/wrong-role negatives, and policy refusal paths execute through maki/db/promotion-test.f; executable PRODUCT count becomes zero; focused typed-local, package, trust, and source gates pass. Smallest owning-path check: bin/hb --load maki/db/promotion-test.f.

Claim: agent=codex-promotion-structure workspace=.jj-ws/habu-migrate-promotion-witnesses-244bd23b
