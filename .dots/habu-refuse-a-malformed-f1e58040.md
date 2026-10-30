---
title: Refuse a malformed quotation at tier 1 with a caught code
status: open
priority: 3
issue-type: task
created-at: "2026-10-30T21:09:40.575258+02:00"
---

Finding (ctrl lane, workspace .jj-ws.noindex/jerry-ctrl): unchecked ': GEKT ( n -- n ) [: 0= if 7 ;] then ;] execute ;' at tier 1 throws E-NELAB-QUOT (-8651), which src/compiler/native/definition.f MALFORMED? does not list, so the load dies uncaught (rc 67) where T0 refuses it rc 70 'ncomp: cannot compile GEKT at then'. E-NELAB-QUOT covers both malformed [: ;] and genuine elaboration gaps, so adding it to MALFORMED? as is would also turn real gaps into refusals: split the malformed case into its own code first. Probe: /tmp/jerry-ctrl-tmp/pr/q2.f. Not a T0 release prerequisite (tier 1 stays for experiments).
