---
title: ROOT! refuses a root one byte under PATH-CAP
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T16:28:40.187555+02:00"
---

Problem: src/core/include.f ROOT! tests a root searchable by appending '/.', so a 1022-byte root, which PATH-CAP (1023) allows, dies 'is not a searchable directory' (root-facts B2b worker, measured with hb --load: 1021 accepted, 1022 refused). Acceptance: every root PATH-CAP allows is accepted or refused by what it is, not by the probe's own suffix. Files: src/core/include.f, test/aot-shifted-bytes.f. Verify: a 1022-byte root case. Depends: habu-reuse-the-obj-a450cc5b. Ownership: those files. Claim: unassigned.
