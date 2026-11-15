---
title: "Take a stored row's signature only from a ( token"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T22:31:34.791101+01:00"
---

Problem: defer and TRUSTED: read '(option)' as the signature 'option' and '(n--n)' as 'n--n' ('defer T9 (n--n)' is rejected at 'n' as no type), while ':' reads '(option)' as a word (': T8 (option) ;' is E-UNDEFINED): one row has two readings. Acceptance: a stored row's signature opens only at a '(' token, as ':' reads it; 'defer T6 (option)' is rejected as 'defer T10 foo' is (E-MISSING-SIGNATURE), and test/signature-nesting.f's 'defer SN-ARD (option)' case pins that. Depends: none. Claim: unassigned.
