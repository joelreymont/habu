---
title: "Public producers for CAD-KIND artifact ids and ART:built"
status: open
priority: 2
issue-type: task
created-at: "2026-07-14T17:39:35.455439+02:00"
---

Evidence/store gap (found by tspolicy lane 2026-07-14): no public producers exist for CAD-KIND:artifact-id / ART:built, so maki/evidence/policy-test.f uses two clearly-labeled TRUSTED.md-rowed fabrication mints (T>AID, T>SID) as the named tested boundary to build test values, and the end-to-end POLICY:CHECK-over-real-bundle path cannot be executed outside white-box cores. Fix: the store/promote owner (habu-v2-typestate-promotion-2266b236 or habu-v2-typestate-store-57afdc0a) exposes typed producers (artifact registration returning CAD-KIND:artifact-id; ART:BUILD returning ART:built with identity threaded - see the identity-threading refinement deferred from the stage sub-dot), then policy-test drops the T>AID/T>SID boundary mints and their TRUSTED.md rows + refine-lint seeds. Acceptance: policy-test builds its values through public producers; fabrication mints removed; end-to-end POLICY:CHECK executed over a real bundle. Files: maki/evidence + maki/store.f or cad.f per owner, policy-test.f, TRUSTED.md, tools/refine-lint-core.f. Verify: maki/test.f, trust/refine lints. Ownership: maki evidence/store.
