---
title: Refuse oversized names in checker callees by code
status: open
priority: 3
issue-type: task
created-at: "2026-10-04T02:50:56.866621+03:00"
---

Problem: capacity dies in the pre-verifier's callees that source reaches, measured by lane 553 ($HOME/.cache/tmp/kestrel-jerry-lexlocb/cd/summary.txt): src/core/checker.f:10312 rc 76 for `package` followed by a 300-byte name (in default stdin mode check.f's own process dies); checker.f:10394 rc 76 for `using` with a 300-byte name; checker.f:10397 rc 76 for 17 nested `using CKU` of a package the file defines; src/core/type-family.f:4921 rc 76 for `: CKQ ( <300 Q>:x -- ) ;`. Lines are on 09299d10+e48885d4. Fix: each limit is refused by a named code at the token, rendered as one located record in every check.f mode, never by killing check.f itself. Acceptance: each probe seen dying first, then one located record per mode; baked: rebuild, g1 == g2, two-generation build. After: 44279554 part B. Tests that depend on the die: tools/check-verify-test-lib.f NO-RESULT and tools/lsp-test-lib.f INCOMPLETE use the `package` long-name die as the input that ends the verifier child without a result line (review 566 F2); when it becomes a refusal, re-point them to an input that still ends the child that way, or show none remains and cover the `exited` arm another way.
