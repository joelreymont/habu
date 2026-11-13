---
title: Record layouts built from offset constants become STRUCTUREs
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T20:02:23.382047+02:00"
---

Problem (fits no census form): records laid out by chains of offset constants ending in a -BYTES constant (src/lib/process.f:85-93), hand cell alignment '7 + $FFFFFFFFFFFFFFF8 and' (src 42, zen 8, maki 3), '8 +loop' strides over cells (src/lib/crypto/evp.f:453), 'CELL *' 645 lines, literal '8 *' 193, 'n cells +' 2479. Acceptance: a census of which are a typed element's width written by hand; each such layout is a STRUCTURE and its strides come from the type. Files: the sites of the patterns above, listed again (the census's site lists are lost). Verify: each file's own test. Depends: none. Census: carl's of 2026-11-11, whose counts are above. Rule: docs/design.md "Writing and changing Habu", docs/forth-card.md section 4 (Joel 2026-11-11: typed containers only). Waits under AGENTS.md's release rule unless a consumer meets it. Ownership: carl. Claim: unassigned.
