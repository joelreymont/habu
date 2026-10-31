---
title: "Design how applications hold other packages' cursors"
status: active
priority: 1
issue-type: task
created-at: "2026-10-31T19:25:36.674072+02:00"
---

E-CAST-OWNER (5b5c118d, a cast into a layout holding another package's OPAQUE structure or cell family, fields included) breaks three applications on the installed hb d5384ec8: Tender (src/questions/saved.f:58-61 four JR cursors per call allocation, DISPOSE-READERS after a throw; tenderd/secrets/azure.f:97 and CLEAR-STAGING ~286, which must wipe a disposed cursor record holding a decoded Key Vault escape but JR's cursor size is private; src/score.f:44, src/admit.f:1359, src/answers.f:46, tenderd/edits.f:160, tenderd/graph.f:96), Maki (src/project-ops.f:177, a TASK:+USER cell cast to a scratch record with two JR:cursor fields: Habu has no typed task-local storage), Zen (src/sql-check.f:76-118 a reader inside a per-parse MEM:ALLOC-BYTES mapping; src/compare.f:15-40 and src/document/markup-pdf.f:175-181 records with two cursors from arena bytes; src/document/structure.f:138/:430 and xlsx.f:93-94 an XML:source-owner cell inside movable byte records, read back later). Already admitted: a STRUCTURE field of type JR:cursor in TYPED-VARIABLE, TYPED-BUFFER or ARENA + AR-NEW storage (model.f call, mcp.f peer). Fable Plan design running (2026-10-31); then the lead's complexity pass, Joel names any public word through the interview, then lanes. Peers stuart (Tender), tim (Maki), phil (Zen, handoff .out/handoff-hb1.md in zen/.jj-ws.noindex/phil-hb) wait on the per-case pattern and run their own earlier hb meanwhile. Ownership: carl. Claim: carl.
