---
title: "Measure ZIP's CRC speed on a large member"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T07:42:45.026943+02:00"
---

Problem: ZIP:READ of one stored member of 2148532224 bytes took 117 s of CPU under bin/hb --load on the Habu installed 2026-11-11 (about 18 MB/s), with 2 s of system time. src/lib/zip.f CRC-FEED folds the CRC one byte a turn through CRC-BYTE for every member read (stored, deflated and replaced alike); that it is the cost is inferred from the code, not measured. Not measured: the same read in a built program, and a deflated member. Acceptance: a profile or timing that names where the time goes; if it is the CRC loop, a table-driven or word-at-a-time CRC, or libzip's own check, with the member-read time before and after. Files: src/lib/zip.f. Verify: the timing of the 2049 MiB read, and the zip suites. Depends: none. Ownership: src/lib/zip.f. Claim: unassigned. Consumers: none reports it; Tender's imports read Office packages of a few megabytes, where 18 MB/s is under a second (not measured in Tender).
