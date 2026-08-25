---
title: Grow checker vectors; freeze guard bodies
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T20:36:18.225545+02:00"
---

Full context: two structural upgrades to the checker-model gate from the audits. (1) 110 published checker results, 14 vectors — grow the table toward the published set in priority order of what a mutation can silently break: the seal (sealed_row_rejects_hidden_underflow), layout width expansion, MATCH payload refinement, the throw edge, quotation Q>XDEAD. (2) The identity gate's frozen-source-body device (a frozen identity word still has its frozen body) caught mutations no numeric vector could reach; the checker gate has no equivalent for CF-PUSH, LIN-CHECK or SUNI — add frozen-body rows for those three. Related precedent: serial exhaustion in the allocator is numerically unreachable so ONLY its frozen body binds it; that pattern is legitimate where reachability is impossible, and these three are reachable so they deserve both.
