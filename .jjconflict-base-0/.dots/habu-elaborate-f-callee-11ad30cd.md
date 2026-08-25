---
title: elaborate.f CALLEE-COPY? guard has no producer
status: open
priority: 2
issue-type: task
created-at: "2026-08-15T18:25:13.858768+02:00"
---

After the staging-road deletion (de637624, master 58463f45), elaborate.f:4228-4229 CALLEE-COPY? holds the recorded inline arity against the word model's - but both now come from the checker, and a mismatched-arity migration is refused earlier by E-NELAB-ARITY (-8303, probed by the staging-road worker). E-NELAB-INLINE at those two lines and E-NELAB-CALL's 'VN @ a <' branch have no reachable producer. Decide with evidence: delete as dead guard, or keep as defence-in-depth with a comment naming why it cannot fire - either way prove reachability first (mutation or probe through the real entry). Both error codes keep other live producers, so error-code-lint stays clean either way.
