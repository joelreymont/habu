---
title: Delete the payload-spelling defect rejection
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T14:37:41.536518+01:00"
---

Problem: src/core/sumtype.f TDECL-VPAY-ARITY (~:976-982) rejects a payload parameter with no declaration spelling as E-TDECL-PAYLOAD with TDECL-DEFECT-FIX$ (:192-193), 'report this declaration as a Habu defect': an engine defect shown as a declaration rejection, a runtime check no test reaches. Acceptance: an input that reaches it is rejected with help that names the fix, or no input can and the branch and TDECL-DEFECT-FIX$ are deleted. Depends: none. Claim: unassigned.
