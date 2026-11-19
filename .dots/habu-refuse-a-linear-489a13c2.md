---
title: Refuse a linear handle a catch left stale
status: active
priority: 2
issue-type: task
created-at: "2026-11-19T13:41:06.312254+02:00"
---

Problem: docs/forth.md:2792-2797 says a linear handle cannot cross a quotation-literal catch (reading it is E-STALE-READ), but tools/check.f accepts reading one after the catch's body consumed it or threw: given package TOK with DEFLINEAR TOK:tok, NEW, FREE, TOUCH ( TOK:tok n -- TOK:tok ) and RISKY ( TOK:tok n -- TOK:tok ) that throws on 3, both `: D ( TOK:tok -- TOK:tok ) [: 3 TOK:RISKY ;] catch {: c :} c 0<> if 1 TOK:TOUCH exit then ;` and the same through `: RUN ( R [ R -- R ] -- R n ) {: q :} [: q execute ;] catch ;` pass the checker at 27bc419d. A throw restores depth, not contents, so the slot holds whatever the body left: for a resource handle (an SQLite statement), a use after free. Acceptance: both forms are refused, reading as E-STALE-READ and dropping as a linear refusal, and the rule's existing cases still pass. Depends: none. Claim: agent=bob workspace=.jj-ws.noindex/bob-linear-catch.
