---
title: Stabilize persisted release signatures
status: closed
priority: 1
issue-type: task
created-at: "2026-10-19T14:15:17.913751+03:00"
closed-at: "2026-10-19T14:21:24.433578+03:00"
close-reason: "Continuing signature oscillation disproved by exact hb2/hb3 decoding: all26697effect/control records are identical and release graphs stayANY. The first-host transition remains causally unproven and no contract change is justified. Sole remaining generation difference is invocation scratch RSRET, tracked under the completed-inference-scratch task."
---

Same-source native generations 21b4c68d produce identical compiled AOT code and parsed map rows but alternate persisted effect graphs for DYNAMIC-STORAGE:RELEASE-RECORD, RELEASE and RELEASE-FIXED between ANY and NULL pointee kinds. UEND changes by 488 bytes; cleared diagnostic scratch remains zero. Exact products are ~/.cache/tmp/habu-core-next.lO8YFmZk/hb and hb2. Establish the responsible checker/instantiation/capture boundary and preserve generic input authority without weakening null/offset rules. Existing engine generation byte equality is the pre-code failure; require focused dynamic-storage and relevant null rejection E2E, exact convergence, full native and independent Astra review. No new scratch resets or graph-value normalization heuristics.
