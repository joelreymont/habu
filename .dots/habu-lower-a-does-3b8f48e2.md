---
title: "Lower a does> definer's wide values at tier 0 under 0 set-check"
status: active
priority: 3
issue-type: task
created-at: "2026-10-24T01:36:25.047269+03:00"
---

After dot habu-lower-wide-values-e122d857 (':' and TRUSTED: bodies), a does> definer under '0 set-check' still lowers a wide value one cell wide at tier 0 ('1 2 7' where tier 1 prints '7 2 1'). It needs an unjudged clause scan at tier 0 (the DECL-DOES-UNJUDGED path tier 1 uses) and the same suspension gate. Found by jerry's nocheck lane, 2026-10-24. Not yet worked.

Lane: workspace .jj-ws.noindex/jerry-doeswide (jerry), reproducers ~/.cache/tmp/jerry-repro.ryKf/3b8-does-*.f.
