---
title: Reclaim code without scanning every record
status: active
priority: 2
issue-type: task
created-at: "2026-11-26T10:43:16.567193+01:00"
---

Problem: a code rewind (CODE-RECLAIM:cp!, called from LINKER:TRUNCATE) scans every dictionary record (src/habu/habu1.f EMIT-DEF-OCC-RECLAIM :2996, src/habu/kernel-x64.f DEF-CODE-RECLAIM-BODY :3245), 12-27% of a Tender placement load on both arches (prof: 93 of 791 samples on pulsar). Acceptance: a rewind visits only records whose code can reach past the new cp, with the same retirements; the placement load's share of cp! falls to near zero on both arches. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-reclaim.
