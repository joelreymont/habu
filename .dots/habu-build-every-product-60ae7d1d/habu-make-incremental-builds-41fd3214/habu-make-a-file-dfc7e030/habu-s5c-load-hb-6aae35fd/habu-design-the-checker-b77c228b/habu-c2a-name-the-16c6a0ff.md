---
title: "C2a: name the rows an effect's text shares"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T20:16:59.535098+02:00"
---

Problem: design r2 D2 (dot habu-design-the-facts-bd8296b9, /tmp/jerry-big/design-facts-r2.md): a word's facts are its effect as text, read back by the checker, so the text must parse to the same effect. src/core/render.f REND-SIG (:507-514) writes the outer rows' cells only, and QREND's T-QUOT arm (:405-418) writes a quotation's row letters only under TAIL-CAPTURE (:614-627), so DIP ( R a [ R -- S ] -- S a ) renders ( a [ -- ] -- a ) and a quotation's return clause renders '| -- ', which the parser refuses; a probe (/tmp/jerry-fid2) found 43 live words affected. Acceptance: one word renders an effect from its rows, naming a row's tail letter wherever its class is not just its own unnamed pair, and a quotation's return clause as '| L -- L'; REND-SIG calls it; diagnostics, JSON packets, SHOW-LOCAL-TYPE and RTAILS render as today. Brief: /tmp/jerry-big/brief-c2a.md. Files: src/core/render.f. Verify: the fid2 probe rerun (every base row that differs by a row name or '| --' becomes same, no same row changes); bin/hb --load test/run.f -- engine checker-soundness diag-position. Depends: none. Ownership: src/core/render.f. Claim: unassigned.
