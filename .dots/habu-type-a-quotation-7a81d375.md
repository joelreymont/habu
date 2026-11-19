---
title: "Type a quotation literal's tail where it runs"
status: active
priority: 2
issue-type: task
created-at: "2026-11-19T15:00:11.762665+02:00"
---

Problem: tools/check.f accepts `: Z8 ( R [ R -- R ] -- R ) {: q :} 5 [: q execute ;] execute drop ;` and, with `: CS-EXEC ( R [ R -- R ] -- R [ R -- R ] ) dup >r execute r> ;`, `: Z11 ( R [ R -- R ] -- R ) 5 swap [: CS-EXEC ;] execute drop drop ;`. Each, called as `BUF [: PEEK ;] Z8` with `: PEEK ( ^u8 -- ^u8 ) dup c@ . cr ;`, runs PEEK over the 5 and dies E-MEMORY-FAULT at address 5: a quotation literal's type loses its tie to the row it runs over (for Z11 inferred: src/core/checker.f QAPP-ROW gives the tail a fresh variable, and RENEW* keeps a row variable inside a cell's quotation type unchanged; Z8's path is unknown). The same lets a nested literal or a captured local carry a freed linear handle out of a catch. Acceptance: Z8 and Z11 are refused, a literal over a row stays accepted where it runs over that row, and a nested literal or captured local that reaches a caught window's open tail leaves it stale. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/bob-quote-tail.
