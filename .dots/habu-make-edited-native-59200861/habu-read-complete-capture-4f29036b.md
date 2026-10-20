---
title: Read complete capture cells directly
status: closed
priority: 2
issue-type: task
created-at: "2026-10-20T21:53:16.334666+03:00"
closed-at: "2026-10-20T22:36:50.307024+03:00"
close-reason: Complete bounded cells use one read; partial tails remain bounded and zero padded. Existing artifact roundtrip, chain capture and app-image checks pass with exact bytes; all tail lengths and a guard-page probe pass. Independent Astra review clear. Matched 4 MiB replay reduced CPU 1.012 to 0.094 seconds.
---

ACAP-CELL@ rebuilds every aligned eight-byte DATA cell through eight byte reads and eight writes to global scratch. A 4 MiB representative scan measured 64.65 ms CPU versus 5.82 ms for a bounded complete-cell read (11.1x), with equal values and partial-tail/page-edge controls. Replace only complete-cell assembly with the existing checked cell read, preserving zero extension above the logical DATA end, pointer admission and artifact format. Verify existing real capture roundtrips, cached/cold engine bytes and changed-path whole-build CPU; independent review before landing. Index clearing saves only 2.7 ms and stays out of scope.

Implemented the bounded read and stack-local partial-tail accumulator. The real artifact roundtrip now checks seven logical tail bytes with poison above DP; pre/post artifacts are byte-identical. The capture suite passes. The identical 4 MiB × 16 probe reports equal values and median CPU 1.011575 s before versus 0.094062 s after (10.75×). Final combined qualification, independent review and landing remain with the integrator; keep this task active.
