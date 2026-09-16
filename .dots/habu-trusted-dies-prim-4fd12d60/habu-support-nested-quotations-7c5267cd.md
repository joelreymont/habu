---
title: Support nested quotations in the JIT load tier
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-09-11T17:00:21.745861+03:00\\\"\""
closed-at: "2026-09-16T14:34:48.011896+03:00"
close-reason: "superseded by habu-campaign-c1-finish-1f129a00: The JIT load tier still refuses lexically nested quotations; residue is removing the representation limit at the responsible layer, not the diagnostic."
---

Unassigned, coordinate with Rowan tier owner. Complete valid reducer: RV:CALLEE ( n -- n ) is1+; `: QBN ( n n n -- n ) [: >r [: >r RV:CALLEE r> + ;] execute r> + ;] execute ;`, inputs3 7 11 expected22. CHECK! certifies the definition and no locals are captured. Current JIT rejects rc75 with one-open-quotation limit (Rowan later makes diagnostic clearer); AOT scanner has a corresponding separate limit being fixed by check_api under habu-diagnose-lexically-nested-56aafc89. Acceptance: diagnose and remove the JIT representation limit at the responsible layer, preserve return stack/frame state and quotation boundaries, compare nested direct/call/loop behavior with AOT, retain capture/effect mismatch negatives. Do not treat accepting the checker declaration as proof of native runtime behavior.


Update2026-09-11 14:15 UTC: Owner now Rowan, .jj-ws/rowan-jit-nest on frozen f115e788. Open quotation state becomes a bounded stack; each closer restores its own frame, overdepth remains a named rejection. Candidate not yet received. The frozen tier review workspace/binary remains untouched.
