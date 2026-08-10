---
title: hb-new lacks the boot stdlib
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T20:04:31.769392+02:00"
---

The dev snapshot's keep surface (tools/build-fixpoint.f BF-APPEND-SNAP-KEEP) mirrors the cold prefix but carries no lib/, and the eight seeded files are deliberately outside PFX-PROVIDE-FILES (which runs unguarded on snapshot boots - the snapshot must never CLAIM what it lacks; require works normally there). Cost: divergence - a program calling STR:LENGTH bare works on bin/hb and fails on hb-new (seeda lane 2026-08-11). Close by adding the eight to BF-APPEND-SNAP-KEEP and to PFX-PROVIDE-FILES together, IN THAT ORDER (keep first, claim second). Acceptance: hb-new runs the bare-STR:LENGTH probe; a snapshot built without the keep still refuses honestly. Files: tools/build-fixpoint.f, src/habu/habu2.f. Depends: habu-seed-the-stdlib-d8e3a757.
