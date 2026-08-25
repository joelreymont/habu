---
title: Lint positive checker error codes
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T00:30:23.624686+02:00"
---

Found during the proofs-master merge (84343a00): both campaigns claimed checker code 7135 for different CAST rules (E-CAST-OWNER vs E-CAST-LINEAR) and tools/error-code-lint.f only checks NEGATIVE codes, so the collision would have passed the gate silently with two rules sharing one number. Extend the lint to the positive checker-code space with the same uniqueness and region discipline; seed it with the current positive inventory. The merge resolved the instance (E-CAST-LINEAR moved to 7137, linear checks ordered before ownership so master's fixtures keep their reject codes) — this dot is the class.
