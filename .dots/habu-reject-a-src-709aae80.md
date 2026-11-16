---
title: Reject a source whose bytes outrun its stamp
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T05:31:26.370545+01:00"
---

Problem: hb --load /proc/self/pagemap never finishes: procfs stats it as a regular file of size 0 that yields bytes without end, so COMPILE-READS:STABLE? accepts its stamp and INCLUDE-READ-ALL (src/core/include.f) reads until EOF, which never comes. A source's stamp names its size, yet the read does not stop at it. Acceptance: a regular file that yields more bytes than its stamp's size is rejected with the 'changed while it was read' record (rc 74) from the load and check.f alike, and /proc/self/pagemap ends that way. Depends: none. Claim: unassigned.
