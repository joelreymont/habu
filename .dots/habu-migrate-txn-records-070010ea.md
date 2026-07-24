---
title: Migrate transaction records to STRUCTURE
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-24T14:02:18.227381+02:00\""
---

Why: maki/db/transaction.f still declares public TX:txn and TX:idem-key through legacy PRODUCT even though unified STRUCTURE owns nominal records. Exact result: replace only both declarations with STRUCTURE. Preserve packages, public identities, txn slot field, idem-key w0/w1/w2/w3 names and order, one-cell and four-cell tagless layouts, TX-TXN and TX-IDEM--KEY generated names/effects, >TXN/TXN>/>IDEM wrappers, canonical encoding, idempotency key bytes, pool lifecycle, validation, errors, and every caller. Retarget transaction-specific PRODUCT comments; leave tx-result SUMTYPE and read-polarity constants unchanged. Owner: package TX and maki/db/transaction.f plus transaction tests. Dependencies: unified STRUCTURE is landed; no ENUM, result, polarity, codec, or storage redesign. Forbidden: alias, cast, parser change, wrapper rename, compatibility declaration, SUMTYPE edit, wire change, hash change, or copied transaction model. Pre-change proof: the Habu source lexer finds exactly two executable PRODUCT declarations in the production file; a representative replacement must pass package-diff lint and the real transaction suite. Acceptance: parent/candidate reflection and generated effects prove txn slot and idem-key four-word field order/layout unchanged; real BUILD, canonical encode, decode, idempotency stability/difference, reordered-set identity, validation refusals, counts, capacity, and buffer paths pass through maki/db/transaction-test.f; keywire process test remains byte-identical; executable PRODUCT count becomes zero; focused typed-local, package, trust, and source gates pass. Smallest owning-path checks: bin/hb --load maki/db/transaction-test.f and bin/hb --load maki/db/keywire-xproc-env-test.f.

Claim: agent=codex-transaction-structure workspace=.jj-ws/habu-migrate-txn-records-070010ea
