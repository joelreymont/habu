---
title: Keep hashing out of the compile hot path
status: open
priority: 2
issue-type: task
created-at: "2026-09-11T16:38:06.276752+03:00"
---

Problem: the symbol filter was 2.9 M SHA-256 calls per load (15.07 s), replaced by FNV-1a in c64808e4 (accepted by cedar 2026-09-11, load 176 to 161 s). Remaining CDIGEST:COMPUTE calls per load: 13,095 (0.45%); every remaining digest in the compile path must be justified as persistence. Acceptance: an audit table of every CDIGEST:COMPUTE caller reached during a forced-tier load with its count and purpose; any digest computed for an in-memory lookup replaced by an equality-confirmed filter; digests at persistence unchanged; canonical persisted digests byte-identical before and after. Files: src/compiler/**. Verify: counter on CDIGEST:COMPUTE over the forced-tier load. Depends: none. Ownership: rowan. Claim: unassigned
