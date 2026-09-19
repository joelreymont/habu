---
title: Adopt wide and parametric typed locals across the codebase
status: open
priority: 2
issue-type: task
created-at: "2026-09-19T18:23:03.521709+03:00"
---

Problem: Joel (2026-09-19): the wide/multi-cell and parametric typed-local features exist to improve the codebase, so every site that destructures a layout value only because a typed local could not hold it must be found and converted. Today src/, lib/, tools/ and test/ carry the workaround shapes the handoff names: unpack-then-repack around a use (X:UNMAKE ... X:MAKE), locals exploded into scalar cells ({: a b c d :} over a two-field value), adapter words that exist only to name pieces, swap/rot juggling around MATCH payloads. Acceptance (alder, after each feature dot lands and its engine is integrated): (1) an audit listing every candidate site by file:line with the shape it exhibits, measured by reading, over src/ lib/ tools/ test/ and, read-only, the consumer trees (loom, maki, kiba, radar, Tender - reported to their owners, never edited here); (2) migrations in commit-sized groups per subsystem, each keeping behaviour (its suites unchanged in what they assert), each reviewed on the line before it chains, converted sites named in the commit body; sites in a live hazel lane (span band 1 files, the native compiler, checker.f) are handed to that lane's owner instead of edited; (3) the card (docs/forth-card.md) and docs/forth.md state the idiom: a domain value stays a named local, destructure only to compute. Depends: habu-bind-a-wide-bc67d207, habu-parse-local-annotations-50be4d43. Ownership: alder (audit and migrations), hazel (review, the lane-owned files). Claim: agent=alder workspace=.jj-ws/alder-review-fixes.
