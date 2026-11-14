---
title: Delete the rotted product-host-identity test
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T17:47:06.115511+02:00"
---

Problem: test/product-host-identity.f fails before it builds (rc 74, 'primitive insertion anchor missing' from INSERT-ROW, :93): its SPEC-ANCHOR$ (:66, '\ ---- primitives whose effect') is no longer in src/habu/prims.f; its ARM anchor (:69) still matches habu1.f. It is not registered in the suite ('run it by hand', :5), so nothing noticed. It checks that a host with one extra primitive builds an identical executable and map through tools/chain-run.f, the old capture build the cross-build replaces (docs/design.md 'Machinery being replaced'). Acceptance: the file is deleted; the property becomes an acceptance case of habu-build-every-product-60ae7d1d ('a host hb with one extra primitive writes the same product, byte for byte'), added to that dot in the same commit. Files: test/product-host-identity.f, .dots/habu-build-every-product-60ae7d1d/habu-build-every-product-60ae7d1d.md. Verify: rg -n 'product-host-identity' over the tree finds no caller. Depends: none. Ownership: carl. Claim: unassigned.
