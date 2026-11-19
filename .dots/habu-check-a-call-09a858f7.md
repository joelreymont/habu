---
title: "Check a call's nested quotation input in linear time"
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T13:57:57.680549+01:00"
---

Problem: checking a call whose input quotation nests k deep (test/split-str-deep.f QUOT-CALL) grows with k squared: master's engine takes 0.24 s at k=400, 0.61 s at 800, 3.68 s at 1600 and 7.72 s at 2047. Measured causes, each a loop over about k items run once per item of another such list: HOLD-I-FROM?'s scan of the instantiable list (src/core/checker.f, workspace carl-holdi); HOLD-OF's loop over the active frames (carl-holdi); LIN-EFF-PASS walking the whole effect once per type var through LIN-VAR-MULT (carl-enmult); RPOS-NAMED? scanning every recorded row pair once per row (src/core/render.f, carl-rpos); HOLD-ESCAPE?'s trail loop, which every enclosing frame repeats over the entries of the frames inside it (dot habu-check-a-quotation-e198e425). Acceptance: the times per doubling of k stay near 2x from 400 to 2047. Depends: none. Claim: agent=carl workspaces=.jj-ws.noindex/carl-holdi .jj-ws.noindex/carl-enmult .jj-ws.noindex/carl-rpos.
