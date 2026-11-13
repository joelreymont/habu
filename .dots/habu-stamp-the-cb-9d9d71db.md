---
title: "Stamp the callback stub into another machine's shadow"
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T15:53:51.441624+02:00"
---

Problem: T0's SHADOWED walk (src/habu/t0.f) seals another machine's copy (x86-64 from arm64) before HOSTED arms NCBSTUB, so a CALLBACK:'s -ENTRY copy for that machine lacks its stub and the host stub pairs with no shadow function; NSHADOW's SOURCE-FUNS drops it from the pairing. Joel chose the callback design on 2026-11-03 on being shown that T0 stamps the stub for both machines. After habu-stamp-the-cb-b7c79edc the seam already stamps the stub on any unplaced seal while armed. Acceptance: STUB-ARM before SHADOWED in LOWER, an x86-64 shadow's -ENTRY copy holds the x86-64 stub, the x86-64 link maps the entry cell to it. Waits: Joel's ruling (2026-11-05): no x86-64 work until arm64-to-arm64 works. Files: src/habu/t0.f, tools/native-emit-x64.f readers. Verify: an x86-64 cross-build's callback on pulsar. Depends: habu-stamp-the-cb-b7c79edc and the arm64-to-arm64 cross-build. Ownership: t0.f LOWER. Claim: unassigned.
