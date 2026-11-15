---
title: Name built locals in a definition text finishes
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T09:00:16.663137+01:00"
---

Problem: when text joins a definition's built items (src/habu/layout.f BODY-ITEMS:RAW-CELL set), src/core/check-walk.f ITEM-WALK? hands the body to the byte walk, CHECK-SCAN-WALK, which reads the listing as source: a rejection there names a built local by its listing token (undefined word 'l9' for a read no group bound, or for an ended local's read), not by its type or the item-walk records MD-LOCAL-ENDED and MD-LOCAL-UNBOUND, and the JSON definition_source of any built definition is the listing ({: l0 :} l0). Acceptance: a rejection in a definition that built items and text both make names each built local as the item walk does, with the same records for an ended or unbound read; definition_source shows no listing token; a build-items test each. Depends: none. Claim: unassigned.
