---
title: Full CL loop macro
status: closed
priority: 1
issue-type: task
created-at: "\"2025-12-29T16:06:22.861987+02:00\""
closed-at: "\"2025-12-30T05:07:30.658812+02:00\""
blocks:
  - habu-4717b9fd7cbcb3d4
---

Implement full CL loop macro with all clauses.
Location: stdlib.habu (complex macro)
Required clauses:
  FOR var FROM x TO y [BY step]
  FOR var IN list
  FOR var ON list  
  FOR var = expr [THEN step-expr]
  FOR var ACROSS vector
  REPEAT n
  WHILE test / UNTIL test
  COLLECT expr [INTO var]
  APPEND expr / NCONC expr
  SUM expr / COUNT expr / MAXIMIZE expr / MINIMIZE expr
  DO forms / DOING forms
  INITIALLY forms / FINALLY forms
  RETURN expr
  WITH var = expr
  NAMED name (for return-from)
Examples:
  (loop for i from 1 to 10 collect (* i i))
  (loop for x in list when (evenp x) sum x)
Blocked by: destructuring-bind (for destructuring FOR)
High priority: enables idiomatic CL code
