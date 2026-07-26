---
title: Migrate action register and dispatch sums
status: active
priority: 2
issue-type: task
created-at: "2026-07-26T12:16:33.112834+02:00"
---

Wave C1 of the unified-type migration program (.blackboard/migration-plan-20260726.md). maki/db/action.f:128 register-result 1 (ok a / incomplete / conflict) becomes a full-mode payload ENUM per the A1 recipe (FIELD name from what action.f stores in the ok arm - justify from source); :137 dispatch-result 0 (5 payloadless arms, MATCHed cross-package by maki/db/agent-loop.f) becomes a COMPACT ENUM under ruling R1 with the live-registry kind pin (the A2 recipe: TK-SUM to TK-ENUM deliberate, pinned so a flip back reds; re-record any shifted baseline consciously - compact sites DO enter the byte-compared census baseline). Constructor spellings byte-identical both families (calibrated verdict tables both trees); consumers untouched, agent-loop suite run explicitly. A1/A2 test patterns; FIELD-removal kill for the payload family; wrong-arm and kind-pin mutations for the compact one. STOP conditions per the program plan; note the 32-byte generated-name cliff when naming twins. Acceptance: action suite, agent-loop suite, maki/test.f green; both diff lints; census verify with the enumerated compact-row delta. Claim: agent=mig-c1 workspace=.jj-ws/habu-mig-c1
