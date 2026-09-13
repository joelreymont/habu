---
title: Append native dictionary publications without a full rebuild
status: open
priority: 2
issue-type: task
created-at: "2026-09-14T00:01:46.985086+03:00"
---

Own native expected-index append primitive, NPUB consumer and focused publication/index tests. Reuse LHIDXADD with pending native parent/DOES ownership and task/seal checks; preserve general ndict! restore/rebuild and rollback. Measured rebuild cost 231–235 us at 15485 records, one rebuild per native ordinary definition; startup address membership scans remain a separate issue. Require actual emitted product behavior and zero ordinary full rebuilds, plus independent Astra review.
