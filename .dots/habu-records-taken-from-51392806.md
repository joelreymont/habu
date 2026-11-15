---
title: Records taken from byte storage by hand state their type
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T20:02:23.369523+02:00"
---

Problem: 112 allocations sized by a -BYTES constant, cells or a -LEN word (src 47, test 8, zen 36, tender 10, maki 11): 71 hold a record the site could name, 14 of them hiding the width in a -LEN word (FD-TABLE-LEN, CHUNK-LEN); 246 private casts ( ptr u8 -- ptr T ) (src 34, test 11, zen 10, tender 147, maki 44), 230 typed-able, 764 calls, most in maki sw-reader.f 18, tender table.f 11, blanks.f 10. Acceptance: each such record is made by the arena's record word or the heap's typed successor with [type], and its cast is gone. Files: the sites of the patterns above, listed again (the census's site lists are lost). Verify: each file's own test; consumers cold equals warm. Depends: none. Census: carl's of 2026-11-11, whose counts are above. Rule: docs/design.md "Writing and changing Habu", docs/forth-card.md section 4 (Joel 2026-11-11: typed containers only). Waits under AGENTS.md's release rule unless a consumer meets it. Ownership: carl. Claim: unassigned.
