---
title: Support nested quotations in the JIT load tier
status: open
priority: 1
issue-type: task
created-at: "2026-09-11T17:00:21.745861+03:00"
---

Unassigned, coordinate with Rowan tier owner. Complete valid reducer: RV:CALLEE ( n -- n ) is1+; `: QBN ( n n n -- n ) [: >r [: >r RV:CALLEE r> + ;] execute r> + ;] execute ;`, inputs3 7 11 expected22. CHECK! certifies the definition and no locals are captured. Current JIT rejects rc75 with one-open-quotation limit (Rowan later makes diagnostic clearer); AOT scanner has a corresponding separate limit being fixed by check_api under habu-diagnose-lexically-nested-56aafc89. Acceptance: diagnose and remove the JIT representation limit at the responsible layer, preserve return stack/frame state and quotation boundaries, compare nested direct/call/loop behavior with AOT, retain capture/effect mismatch negatives. Do not treat accepting the checker declaration as proof of native runtime behavior.
