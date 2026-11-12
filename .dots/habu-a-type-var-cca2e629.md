---
title: A type variable refuses a nominal pointee and says not why
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T13:23:35.945743+02:00"
---

Problem: a word declared ( ptr a -- n ) refuses a pointer to any nominal family: with STRUCTURE pt, TYPED-VARIABLE SV pt and : D ( ptr a -- n ) NULL-PTR - ;, the body SV D prints 'habu: in t2: at 'D' expected: ptr a actual: ptr pt ; suggestion: Change the body so produced types match the signature.' on the installed hb; BYTE-VIEW and the null stack's PTR>N refuse ptr option<n> and ptr nullable<u8> the same way. docs/forth.md:2564 states the rule (a free type variable cannot unify with a nominal enum or family) for result's error variable; the refusal neither names the rule nor the family, and its suggestion is wrong. The limit forces workarounds: test/storage-binding-e2e.f in the null stack records each nullable cell's address before allocation because PTR>N cannot take it. Acceptance: the refusal names the rule, the variable and the nominal family it would bind, and a remedy that works; and the limit is either removed where binding a nominal pointee cannot forge a value (an address read as a number, a distance) or its reason is stated beside the rule in docs/forth.md. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-nominal.
