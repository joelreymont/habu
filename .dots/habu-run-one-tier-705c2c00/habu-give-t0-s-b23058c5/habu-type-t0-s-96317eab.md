---
title: "Type T0's emission state"
status: active
priority: 2
issue-type: task
created-at: "2026-11-02T19:51:43.948200+02:00"
---

Lane S3 of /tmp/jerry-big/t0state-design.md (revision 4). Problem: T0's emitter seam (src/habu/t0-emit.f) holds its phase in PLACED-AT's -1, so EMIT-PLACE or EMIT-SEAL with no emission open is not refused, and EMIT-CB-STUB guards an unreachable armed-stub-on-unplaced-seal case (545; the C1+C2 review's finding). Acceptance: private ENUM emission shut/open/placed; EMIT-OPEN any->open, EMIT-PLACE open|placed->placed (shut refused), EMIT-SEAL open|placed->shut (shut refused) clearing PLACED-AT before anything throws; EMIT-CB-STUB ( -- n ) only on a placed seal; SLOT-AT and CUR unchanged; no public word changed. Files: t0-emit.f. Verify: brief /tmp/jerry-big/t0s3-brief.md (twin row, C2 rows, x86 cross-build, pulsar rows). Depends: none. Ownership: t0-emit.f. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-s3.
