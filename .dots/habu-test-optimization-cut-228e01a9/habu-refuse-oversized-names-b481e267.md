---
title: Refuse oversized names in checker callees by code
status: open
priority: 3
issue-type: task
created-at: "2026-10-04T02:50:56.866621+03:00"
---

Problem: capacity dies in the pre-verifier's callees that source reaches, measured by lane 553: src/core/checker.f:12207 rc 76 for 17 nested `using CKU` of a package the file defines; src/core/type-family.f TFQ-FOLD-COPY (:3904) rc 76 for `: CKQ ( <300 Q>:x -- ) ;`. Fix: each limit is refused by a named code at the token, rendered as one located record in every check.f mode, never by killing check.f itself. Acceptance: each probe seen dying first, then one located record per mode; baked: rebuild, g1 == g2, two-generation build. After: 44279554 part B.
