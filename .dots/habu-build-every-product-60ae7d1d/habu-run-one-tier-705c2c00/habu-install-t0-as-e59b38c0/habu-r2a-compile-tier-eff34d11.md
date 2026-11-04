---
title: "R2a: compile tier 0 through T0 on arm64 (COMP cell, latch 2)"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-30T17:12:40.646550+02:00"
closed-at: "2026-10-31T12:12:38.664483+02:00"
close-reason: "Landed with the T0 release (ttloqwsx b17a1ee3): full suite 510/510, two generations converge"
---

Workspace .jj-ws.noindex/jerry-r2 on R1 (ulzktzrp). Brief /tmp/jerry-big/t0-r2a-flip-brief.md: NCOMP-DISPATCH:COMP-CELL $3838 (layout, claims, native-layout literal, cold-boot zero, reloc mark); TIER-LATCH emitter (1 if TIER-CELL 1; 2 if TIER 0 and COMP set; else 0) used by TIER-COLON-DISPATCH, DEF-OPEN, BTICKORDER; LOAD reads COMP on latch 2; DEF-CLOSE, BNDAPPEND, engine-origin NATIVE-ARM accept nonzero; T0:DISPATCH! writes COMP; new src/habu/runtime-arm64.f fills CHECKER-REG:SEAL-TARGET with SELECT AARCH64 + T0:INSTALL, SEAL calls SEAL-TARGET on every target; t0-install.f, t0-child.f and their row deleted. Placeholders for Joel: COMP-CELL, TIER-LATCH, runtime-arm64.f/RUNTIME-ARM64/INSTALL-T0/BIND, R1's LOAD-T0. Probe /tmp/jerry-big/r2/r2-probe.f (dies rc 67 on R1). Next: R2b build hand-back (save/restore host XT/COMP/callback cells around the window).
