---
title: Let library tokens carry their records
status: open
priority: 2
issue-type: task
created-at: "2026-11-22T12:11:01.329776+01:00"
---

Problem: XML:reader (src/lib/xml/state.f:254), XML:source (src/lib/xml/source.f:63), JSON:reader (src/lib/json/read.f:193), CMD:command (src/lib/process-command.f:129) and PROCESS:request (src/lib/spawn.f:31) mint from and erase to ^n, then cast that to their record (process-command.f:62, spawn.f:47 >VECTORS), a claim the checker does not prove. LINEAR: now carries a pointer to a closed record (test/linear-suite.f LR1-LR2), as EDIT does (src/lib/byte-edit.f:41). Acceptance: each token mints from and erases to a pointer to its own STRUCTURE; no cast or offset view reaches its state. Depends: none. Claim: unassigned.
