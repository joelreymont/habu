---
title: "C2c-i: grow the render buffers with their text"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-07T03:21:50.290334+02:00"
closed-at: "2026-11-07T04:02:42.587509+02:00"
close-reason: "implemented, reviewed, landed with C2c (master after b05b406c): gate chain /tmp/jc.7p fixpoint at gen2 rc 0 (real 445 s), full suite 293 of 293 PASS rc 0 (/tmp/jt.yi/suite.log)"
---

Problem: render.f RSBUF is a fixed 16 KB DATA buffer; EMIT-RAW throws RDIAG:E-DIAG-CAPACITY (-2901) uncaught once an effect's text passes it, so the facts check kills the load of a declared word with a longer text (/tmp/rv-c2c.fS/vrmid.f W3, about 18 KB, rc 67) that master certifies. Acceptance: RSBUF, RPOS and TCAP are mapped on first use, each grows when it fills, all three are released at snapshot (REND-SNAPSHOT-RESET); RSBUF-FULL and snap-lib.f's RSBUF zeroing go; engine-suite RSW3C passes. Brief: /tmp/jerry-big/brief-grow-render.md. Files: src/core/render.f, src/habu/snap-lib.f, test/engine-suite.f. Verify: candidate build within 5 minutes; vrmid.f and vrbig.f rc 0; fid-new probe; size -m DATA; the named tests. Depends: none. Ownership: those words. Claim: agent=worker workspace=.jj-ws.noindex/jerry-grow.
