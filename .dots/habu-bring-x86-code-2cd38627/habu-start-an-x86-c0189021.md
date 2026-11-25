---
title: Start an x86 process without copying the region
status: closed
priority: 2
issue-type: task
created-at: "2026-11-25T15:43:45.741686+01:00"
closed-at: "2026-11-25T17:17:42.202631+01:00"
---

Problem: every x86-64 program starts 8 to 10 ms slower than on arm64: src/habu/boot-x64.f REGION-VIEWS, copies [DBASE, CP), and CP is already DBASE + DICT-SIZE, so it copies 6.3 MB of zeros (3,149 page faults, 7,584 KB resident against 54 and 756 KB). Acceptance: the region is mapped from its memfd before anything writes it, as arm64's EM-MMAP-CODE-REGION does, and an empty program's page faults and start time are measured before and after. Depends: the data stack lane. Claim: agent=worker workspace=.jj-ws.noindex/jerry-x-boot.
