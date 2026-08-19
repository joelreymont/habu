---
title: "Route 3: the type foundation loads post-hook, checked"
status: open
priority: 2
issue-type: task
created-at: "2026-08-19T10:53:19.909085+02:00"
---

The zero-trust route for the 485 recording-gap TRUSTED: sites (fab55650's blocked set, epic 4fd12d60): src/habu/habu2.f:861-890 loads type-schema.f, type-family.f, sumtype.f and checker.f BEFORE src/core/check-hook.f installs the hook, so their : definitions record no signature - measured 2026-08-19 by trusted-1 through the real prefix build (ED-PROBE rc 70 evidence on the leaf of fab55650). Move the type foundation post-hook so its signatures are DERIVED AND CHECKED - the bootstrapping knot is the checker checking its own foundations, which is the same territory as habu-seal-the-checker-5314c0ab (package ownership of checker.f) and the dissolved umbrella habu-tfam-2b-sealed-1b77662c (re-derive before dispatch). Rejected alternatives, with reasons on fab55650's leaf: owner-side declared-signature recording (trust that route 3 would delete), mass PRIM axioms (contradicts the epic by its own text). This blocks fab55650's remaining 485 sites and therefore feac682b (the reader deletion).
