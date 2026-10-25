---
title: Take ARM64 capture reach from emitter rows
status: open
priority: 2
issue-type: task
created-at: "2026-10-25T22:57:08.843244+02:00"
---

Rule: docs/architecture.md:160-164 and :264-266, a cell reaches the image as an address only because a declaration, a claim or a code row names it; src/habu/aot-capture.f:1706-1707 says address sites never come from recognizing instruction bytes. Problem: the ARM64 capture still decodes captured code bytes, with two decoders for the same B/BL imm26 (ACAP-W32@, ACAP-TGT, ACAP-CALL?, ACAP-BRANCH? ~78-92; ACAP-GRAPH-BRANCH?, ACAP-GRAPH-TARGET, ACAP-GRAPH-PC-KIND, ACAP-GRAPH-PC-TARGET ~1130-1159) and several passes over the blob from CAPTURE-PREPARE/CAPTURE-COMPLETE: ACAP-SCAN-CALLS (~1697), ACAP-GRAPH-SWEEP/-GAPS (~1258-1292), ACAP-SHADOW-REACH/-SCAN (~2066-2163, 'walks the same code from the same roots'), ACAP-GRAPH-PATCH (~1323-1350). x86-64 CAPTURE-NATIVE (~2813) already takes reach from the compiler's recorded routine graph. Acceptance: ARM64 call and branch edges come from the emitter's records as on x86-64; both byte decoders and the extra sweeps are deleted; captured images are byte-identical or every difference is explained. Files: src/habu/aot-capture.f, the ARM64 emitters' record writers. Verify: test/run.f, two-generation build, AOT image rows.
