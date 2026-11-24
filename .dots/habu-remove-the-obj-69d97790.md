---
title: Remove the object folders of older hbs
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T17:35:45.488746+01:00"
---

Problem: each hb writes its objects under <out>/obj/<its stamp>/ (src/lib/build-out.f); nothing removes the folders of older stamps, so an out root grows with every new hb. Acceptance: an out root keeps the objects of the hbs that build there and no others, without one hb deleting objects another still uses. Design: needed. Depends: none. Claim: unassigned.
