---
title: Reject an overflowing number in a type entry
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T19:12:11.797959+01:00"
---

Problem: src/core/type-family.f ER-DIGITS (:3477) does not reject a digit run past a cell, so an entry text holding 9223372036854775808 reads as MIN-INT through ER-NUM and ER-HEAD/ER-VARIANT; the round trip at ER-READ (id ENTRY-TEXT$) then makes ET-NUM (:3286) die 'type entry number out of range'. Acceptance: ER-DIGITS answers ER-BAD on overflow; ET-NUM writes every number with no die. Depends: none. Claim: unassigned.
