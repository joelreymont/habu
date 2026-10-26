---
title: Record every loader action as an edge
status: active
priority: 1
issue-type: task
created-at: "2026-10-26T13:52:38.596489+02:00"
---

Joel 2026-10-26: every require/required/include at a loader cut writes one EV-LOAD row and a warm load performs the edge; no skip records, no provided mode. Cache format 24 -> 25. Lane A1 records edges (design sections Row, Observer, NUNIT-OBJECT); lane A2 then deletes section 8 and the require counting. Design ~/.cache/tmp/carl-file/edge-design.md, brief brief-edge1.md. Major review (persisted format, recovery). Workspace: .jj-ws.noindex/carl-edge. Claim: agent=carl worker.
