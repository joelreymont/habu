---
title: The check spells a shadowed private word folded to lower case
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T17:36:21.403474+01:00"
---

Problem: for a public colon word that binds a private word of another stack effect, the check's E-SHADOWED-ARITY record names the private word `e` where the load's names `E`. Reproducer: /tmp/cbrev-A62H/p3/shadow-colon.f. Acceptance: the check's record equals the load's. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-shadowcase.
