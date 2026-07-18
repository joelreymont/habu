---
title: "Foundation A1b: package-scoped nominal role resolution"
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T17:42:45.520291+02:00"
blocks:
  - habu-choose-extent-nominal-f61dac3e
---

Blocked fork split out of habu-foundation-a1-declarable-98aebe7b. The CT-ROLE declaration mechanism (DEFTYPE) already mints distinct nominals past CC-MAX with converters, persistence, and rollback (proven by test/type-nominal-suite.f). The remaining requirement - package-scoping - requires making the con-resolution path package-aware (CON-OF/CT-FIND at checker.f:2432, TYPE-RESERVED? at checker.f:2511, DEFTYPE name storage in roles.f:90, converter naming, CT-SNAPSHOT-PERSIST name format at checker.f:5656). Today DEFTYPE registers a global bare name and a second package declaring the same tail hard-dies (exit 70). Decide package-first-bare vs qualified-only semantics (mirroring SIG-FAM?/TFAM-RESOLVE package scoping for families), implement, add negative fixtures (two packages same tail distinct; bare tail unresolved outside its package), prove byte-fixpoint + full gate. Must resolve the substrate decision first (see blocks).
