---
title: Say why a 256-field structure is refused
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T13:34:29.155594+02:00"
---

Problem (review of carl-self, probes ~/.cache/tmp/rev-selfptr/v255.f, v256.f): a STRUCTURE with 256 fields is refused with only 'declaration failed'; 255 declare. Acceptance: the refusal names the field limit and the field at which it is passed, in the declaration packet (E-CAP or the code the cap uses elsewhere), with a test case. Files: src/core/structure-decl.f and the limit's owner. Verify: v256 under --load and tools/check.f --json-errors. Depends: none. Ownership: structure-decl.f. Claim: unassigned.
