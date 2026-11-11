---
title: Records laid with create and allot become typed buffers
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T20:02:23.343349+02:00"
---

Problem: 3230 create/variable-plus-allot sites (src 963, tools 334, test 591, zen 174, tender 1070, maki 98); of a 60-site sample 19 hold a record or typed element the site could name, 31 are byte rows, 2 foreign layouts, 4 unclear. Most typed-able: src/core/checker.f 87, tender answering.f 38, test/gate-pool.f 33 (pattern match, not read). Acceptance: each site holding a typed element is a TYPED-BUFFER or TYPED-VARIABLE (they exist); byte rows and foreign layouts stay. One brief per few named files. Files: lists 3-*.txt. Verify: each file's own test; cold equals warm. Depends: none. Census: ~/.cache/tmp/carl-census/census.md (2026-11-11) and its site lists. Rule: docs/design.md "Writing and changing Habu", docs/forth-card.md section 4 (Joel 2026-11-11: typed containers only). Waits under docs/gate.md "Until Zen, Tender and Maki are released" unless a consumer meets it. Ownership: carl. Claim: unassigned.
