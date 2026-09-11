---
title: Keep hashing out of the compile hot path
status: closed
priority: 2
issue-type: task
created-at: "\"2026-09-11T16:38:06.276752+03:00\""
closed-at: "2026-09-11T17:55:46.341403+03:00"
close-reason: "Audited on 41df9051 with per-site counters: CDIGEST:COMPUTE is the only SHA-256 caller in a forced-tier load (engine SHA256 delta 2,844,520 = CDIGEST total; SHA256-FILE 0; TF-SHA16 0). 2,831,875 calls were the symbol filter (15.0 s, removed by 7cb6f4c2/c64808e4); the remaining 12,645 (195 ms, 0.11%) hash each definition's own source text: 7,867 IR-SOURCE registrations and 4,778 SOURCE! equality verifies; twelve digest sites have no caller outside tests. Nothing persists on the compile path; source.f retains no bytes, so no lazy digest; SOURCE! cannot equality-confirm, so no cheap filter. Tier 0 does zero digests. Measured negative; the SOURCE!+ADD-SOURCE double hash is tracked as habu-digest-a-def-6fac0fe7."
---

Problem: the symbol filter was 2.9 M SHA-256 calls per load (15.07 s), replaced by FNV-1a in c64808e4 (accepted by cedar 2026-09-11, load 176 to 161 s). Remaining CDIGEST:COMPUTE calls per load: 13,095 (0.45%); every remaining digest in the compile path must be justified as persistence. Acceptance: an audit table of every CDIGEST:COMPUTE caller reached during a forced-tier load with its count and purpose; any digest computed for an in-memory lookup replaced by an equality-confirmed filter; digests at persistence unchanged; canonical persisted digests byte-identical before and after. Files: src/compiler/**. Verify: counter on CDIGEST:COMPUTE over the forced-tier load. Depends: none. Ownership: rowan. Claim: unassigned
