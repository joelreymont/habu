---
title: Cross-build the x86_64 engine and gate it on the peer
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T17:32:42.544466+03:00"
---

Problem: docs/x86-64.md (campaign habu-campaign-c6-targets-86bb56bb): order is cross-build first, self-host second: the arm64 engine compiles Habu for the x86_64 contract and writes an x86_64 ELF, and the Intel machine runs it as a device peer the way the serial peers do. Acceptance: tools/build-fixpoint.f (or tools/native-build.f) takes the x86_64 contract and writes an x86_64 engine image from the arm64 host, with the x86_64 primitive bodies from the primitive table; a device-peer gate under test/ ships the image to the Intel machine, runs test/run.f there and reports the result here; docs/bootstrap.md records the recovery rule for x86_64 (cross-build from a working arm64 engine; no Gforth mirror). Files: tools/build-fixpoint.f, tools/native-build.f, src/habu/habu1.f (x86_64 bodies) or the table's backend files, test/ (peer gate), docs/bootstrap.md. Verify: the cross-built engine boots on the Intel machine and test/run.f runs there. Depends: the OS seam, the lowering. Ownership: Joel (x86_64 lane). Claim: unassigned.
