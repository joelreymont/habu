---
title: Verify a replay opened inside a definition
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T14:54:26.687057+01:00"
---

Problem: a parse-imm immediate that calls CHECK:SCOPED or VERIFY:SOURCE-BUF-IN-SCOPE inside a definition is rejected E-VERIFY-WINDOW rc 70 under --load and check.f (checker.f PENDING-REJECT), but --verify-only gives rc 0 with no record (/tmp/carl-pd/p/p1, p2), and no test holds an E-VERIFY-WINDOW record. Acceptance: the three readers agree on p1 and p2; one test pins the outcome. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-vw.
