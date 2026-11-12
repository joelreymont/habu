---
title: Records laid with SCRATCH-ALLOT become CWORK buffers
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T20:02:23.355837+02:00"
---

Problem: 23 'create R n SCRATCH-ALLOT' sites, all in src: 14 hold a record the site could name (t0-arm64.f 3, task.f 3, process-env.f 2), 2 foreign layouts, 7 are the definers. Acceptance: the 14 are CWORK-BUFFER or the typed scratch form. Files: list 4-scratch-allot.txt. Verify: each file's own test. Depends: none. Census: ~/.cache/tmp/carl-census/census.md (2026-11-11) and its site lists. Rule: docs/design.md "Writing and changing Habu", docs/forth-card.md section 4 (Joel 2026-11-11: typed containers only). Waits under AGENTS.md's release rule unless a consumer meets it. Ownership: carl. Claim: unassigned.
