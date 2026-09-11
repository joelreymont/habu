---
title: Diagnose lexically nested quotation elaboration rejection
status: open
priority: 1
issue-type: task
created-at: "2026-09-11T16:51:00.123952+03:00"
---

Unassigned; discovered by /root/check_api while testing quotation-frame repair 5137130d on cfcf1baf. Exploratory reducer `[: >r [: >r RV:CALLEE r> + ;] execute r> + ;]` rejects -8651 during elaboration before allocation. This is distinct from spills inside quotation function bodies: runtime nesting via a named callee passes after that frame fix. Acceptance: record full typed enclosing definition and inputs, reduce and determine whether declaration or compiler is wrong, fix responsible layer if valid, and add a real load-path regression without broadening frame repair. Current evidence is the agent's exploratory report, not an independently reproduced root diagnosis.
