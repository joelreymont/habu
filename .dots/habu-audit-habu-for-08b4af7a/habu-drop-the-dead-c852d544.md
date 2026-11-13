---
title: Drop the dead layout bands
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T11:00:44.499883+02:00"
closed-at: "2026-11-13T12:13:12.853303+02:00"
---

Problem: dead space below DATA stays because moving DATA's start is refused: the JIT-SNAP band (layout.f :1358-1376, PD-TABLE-OFF :1509), the evaluate frame's $140-$157 gap, and the callback row's unused fourth cell (layout.f:527 CB-ROW-BYTES, task.f ROW-BYTES). Acceptance: all three are gone and the engine builds through one stage. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-dstart
