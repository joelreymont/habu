---
title: Refuse a defer into a constructor package
status: active
priority: 2
issue-type: task
created-at: "2026-11-14T15:49:13.089553+01:00"
---

Problem: `STRUCTURE scprot FIELD x n FIELD y n ;STRUCTURE` then `defer SCPROT:EXTRA ( -- n )` loads rc 0 under hb --load: no constructor-package guard (CTOR-EXTEND?-XT) runs for a defer's name, while `: SCPROT:EXTRA ( -- n ) 1 ;` is refused (throw 7111, rc 67). Acceptance: a defer naming a tail in a constructor package is refused like the colon definition is. Depends: none. Claim: agent=jerry-worker workspace=.jj-ws.noindex/jerry-dfg.
