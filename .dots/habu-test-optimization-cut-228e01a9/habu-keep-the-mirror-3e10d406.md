---
title: "Keep the mirror's BEGIN frames off its cells"
status: closed
priority: 2
issue-type: task
created-at: "\"2026-10-01T05:06:16.911901+02:00\""
closed-at: "2026-10-01T12:50:20.158065+02:00"
close-reason: Landed as ab666bb6 (r4-mirror 49d0c3dc); Fable review ACCEPT
---

Problem: bootstrap/cg/jit.fs:635-636 places the Gforth mirror's BEGIN snapshot stack at SNAPSTK-OFF $360, 28 frames of 24 bytes ending at $600, which covers the mirror's own LASTC-CELL $560, RSP-CELL $568, EXITH-CELL $570, LVD-CELL $578 and LVH-OFF $580..$600 (bootstrap/cg/forth.fs:370-377). A BEGIN nest 22 or more deep in mirror-compiled source overwrites them. The native engine moved these frames to SNAP-RELOC:XTCELL-END (src/habu/layout.f:1832) and has no overlap; native bands are checked by CLAIMS-ASSERT (src/habu/data-claims.f), the mirror's are not. Found by the r4-lvf lane (on yrykozmu, after it moved LVF-OFF to $6D0 in both). Acceptance: the mirror's BEGIN frames sit in a band no other mirror cell uses, at the native engine's placement where the mirror's layout allows; a case compiled by the mirror with a BEGIN nest deeper than 21 that fails first (wrong output or crash) and then compiles and runs correctly; Gforth recovery check passes (HABU_ALLOW_BOOTSTRAP=1 HABU_BOOTSTRAP_CHECK_ONLY=1 tools/bootstrap.sh). Files: bootstrap/cg/jit.fs, bootstrap/cg/forth.fs if a band moves. Verify: the new case, Gforth recovery check, native build convergence unchanged. Depends: habu-give-lvf-a-77abc1f6 (r4-lvf). Ownership: mirror BEGIN snapshot band.
