---
title: "Close T0's remaining malformed-body gaps"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-31T10:08:13.156506+02:00"
closed-at: "2026-10-31T12:11:57.987219+02:00"
close-reason: "Landed with the T0 release (ttloqwsx b17a1ee3): full suite 510/510, two generations converge, edit matrix F53/F105/F156 only, as master"
---

Problem: fold-in review of ttloqwsx (the t0fa and t0fb lanes squashed): MATCH over an unknown family compiles with the MATCH dropped (rc 67 after wrong output; the JIT refuses rc 70); an unknown MATCH variant is named at OF; an unjudged number compiles as 0 ({: 1:n :} [: 1 ;]); TRUSTED: dead code (1 exit DUP) refused with a false E-UNDEFINED; is/['] without a target print E-UNDEFINED. Acceptance: /tmp/jerry-big/t0fc-brief.md Requirements, rows in test/load-reject-diag-test.f failing on the ttloqwsx candidate and passing after. Files: src/core/checker.f, src/habu/t0.f, src/compiler/native/definition.f, test/load-reject-diag-test.f. Verify: brief's Proof. Depends: none. Ownership: lane t0fc. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-t0fc
