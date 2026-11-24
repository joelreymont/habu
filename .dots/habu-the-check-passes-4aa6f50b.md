---
title: "The check passes a FUNCTION: or CALLBACK: word that shadows a private word with another effect"
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T17:36:21.392342+01:00"
---

Problem: the load rejects E-SHADOWED-ARITY when a public FUNCTION: or CALLBACK: word (or a word CALLBACK: generates) binds its package's private word of another stack effect; tools/check.f --verify-only passes it (rc 0, no record). Reproducers: /tmp/cbrev-A62H/p3/shadow-fn.f, /tmp/cbrev-A62H/p2/dup-private.f. Acceptance: the check's record equals the load's for both. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-shadowfn.
