---
title: Hash native keys by complete cells
status: closed
priority: 2
issue-type: task
created-at: "2026-10-20T21:54:55.834889+03:00"
closed-at: "2026-10-20T22:36:51.078539+03:00"
close-reason: Private buckets hash bounded complete cells and byte tails; full serialized key comparisons remain authoritative. Actual-key replay produced identical rows across 1.56 million lookups with CPU 5.798 to 2.237 seconds. FILE state, prepared boundary, reclaim and replay E2Es pass; independent Astra review clear.
---

A representative cache contains 312123 dependency lookups with 353-byte mean keys. Replaying five passes through the production hash costs 5.798 CPU seconds before and 2.237 after bounded complete-cell hashing, with both exact comparisons and every resolved row preserved. The 4096-bucket table, portable bytes, identity and occurrence checks remain unchanged; KEY-RECORD equality still protects stale projected zero-token rows. Existing FILE preparation, state, redefinition, explicit reclamation, replay and path-shadow E2Es pass. Keep active until the whole edited-build comparison, independent review and final qualification pass.
