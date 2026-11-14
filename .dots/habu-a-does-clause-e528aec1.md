---
title: "A does> clause's data field holds what no storage holds"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T22:23:08.856894+02:00"
---

Problem: src/core/check-walk.f DOES-DIN (:1713) gives a does> clause's data-field pointer a plain fresh pointee, so the clause's declared row retypes whatever the creating word compiled with no storage check. Probes (rc 0 under bin/hb --load): does-ptrw.f ': MK ( n -- ) create , does> ( -- ptr width<n> ) ;  7 MK X' then 'X @ width>n' prints 7 (a width forged from an integer); does-ptrxt.f ( -- ptr xt ) and does-ptrptr.f ( -- ptr ptr u8 ), the same MK with that clause and '7 MK X', load, so they forge an xt and a pointer the same way. Acceptance: a does> clause's data-field pointee meets the storage gate (src/core/checker.f STORAGE-TERM-OK? :8245: no width, xt, scheme or hidden term) and the raw-cell fence that refuses a pointer held in raw bytes, checked after CHECK-DOES-RUN (src/core/check-walk.f:1854); the three probes are refused with E-BAD-STORAGE or the raw-cell code; a clause typed ( -- ptr n ) or ( -- ptr u8 ) still loads; the docs' claims that no storage holds a width or an xt (docs/forth.md width and xt bullets, docs/forth-card.md) are true. Depends: none. Claim: unassigned, owner carl.
