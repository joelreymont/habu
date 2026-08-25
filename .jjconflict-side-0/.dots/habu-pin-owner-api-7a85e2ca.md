---
title: Pin OWNER-API wordlists in stage0 EMIT-PROTWID
status: open
priority: 2
issue-type: task
created-at: "2026-08-01T17:23:55.314385+02:00"
---

bootstrap/cg/forth.fs:1393 EMIT-PROTWID answers protected-WID membership from the bitmap ALONE. Its production twin, src/habu/habu1.f EMIT-PROTWID, first compares the wid against OWNER-API-PUB-WID (1) and OWNER-API-PRI-WID (2) and answers 'protected' by RULE, so those two engine-reserved wordlists are protected identically on every boot path and cannot be forged into. The gforth recovery engine therefore has a weaker seal than the engine it recovers: '1 set-current : X ;' and '2 set-current : X ;' would publish instead of exiting 84 (the forges test/seal.f SLV-OWNER-FORGE covers on the real engine). This divergence predates the WID-indexed bitmap - the table-era stage0 had the same shape - and the bitmap work preserved it rather than widening its scope. Fix: add the two CMPI/BCOND pins to the stage0 routine ahead of the bound check, exactly as habu1.f has them, and prove it by bootstrapping an engine with tools/bootstrap.sh and running the two SLV-OWNER-FORGE cases against it. Blocked in practice by habu-fix-stage0-pre-88a4297e: tools/bootstrap.sh is currently red on an unrelated stage0 mirror defect, so the proof leg cannot run until that is repaired.
