---
title: Call check-does! through the owner record
status: open
priority: 2
issue-type: task
created-at: "2026-10-23T20:03:26.930193+03:00"
---

Tier 0's C-CALL-CHECK-DOES (src/habu/habu2.f ~3126) finds check-does! with package-scoped LFIND, so a package-private 'check-does!' in user code is called instead of the checker's (a probe shadow printed SHADOW, reviewing slnrrlyu 2026-10-23). Tier 0 reaches owner operations through the record (DECL-OWNER:FIND); slnrrlyu moves check-unjudged! there. Acceptance: the shadow probe no longer runs the user's word at tier 0; a test row; the by-name label and string gone from habu1.f, habu2.f and the seed. Claim: unassigned.
