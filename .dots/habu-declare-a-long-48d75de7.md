---
title: Declare a long pointer chain of families
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T12:57:24.520294+01:00"
---

Problem: 3000 families each holding a pointer to the previous, then TYPED-VARIABLE V s3000 (/tmp/deepcut-probe/u1.f), exit 76 'checker: term walk too deep (cyclic term)'; 1000 is fine; the walk is not identified. Acceptance: the chain at 3000 and 20000 families declares and is used, or is rejected at its place with the limit and a fix; a test at the old failing count. Depends: none. Claim: unassigned.
