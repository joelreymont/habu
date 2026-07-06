---
title: Retire TLP-MK2/UN2/MK4/UN4 TRUSTED seed makers on TFAM 8/9
status: open
priority: 2
issue-type: task
created-at: "2026-07-06T22:22:23.942547+02:00"
---

test/type-layout-lower-pending.f defines TRUSTED: TLP-MK2 ( -- tlp-res<n,n> ) / TLP-UN2 / TLP-MK4 ( -- tlp-mix<n,n> ) / TLP-UN4 (TRUSTED.md rows 360-363, added 2026-07-06) to seed and unpack raw layout bundle cells for the pass-2 execution rows. These are a named unchecked boundary standing in for real constructors/destructors. They are in TENSION with docs/type-families.md 25.5, which states the invalid-tag / seeding test entry must seed payload cells 'using only the existing image-writer trust rows' and 'Do not introduce any new ADT TRUST, TRUSTED:, set-check, or TRUSTED.md row to forge payload slots plus an invalid tag' — yet TLP-MK*/UN* are exactly new TRUSTED: rows forging payload+tag cells. Per the Habu rule 'unchecked boundary removed when the capability lands': when TFAM item-8/9 land real checked variant constructors and destructors (e.g. OK/ERR builders and MATCH/field destructuring that produce/consume tlp-res<n,n> in checked Habu), rewrite the type-layout-lower-pending execution rows to build and unpack bundles through those checked constructors, then delete the TLP-MK2/UN2/MK4/UN4 TRUSTED: definitions and their TRUSTED.md rows (360-363). Acceptance: no TLP-MK*/UN* TRUSTED rows remain, the execution rows still prove whole-bundle transport preservation via checked constructors, and the 25.5 seeding rule holds with no new ADT trust.
