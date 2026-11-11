---
title: "A does> clause's data field holds what no storage holds"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T22:23:08.856894+02:00"
---

Problem: src/core/checker.f DOES-DIN (:26286 on master) gives a does> clause's data-field pointer a plain fresh pointee, so the clause's declared row retypes whatever the creating word compiled with no storage check. Probes (~/.cache/tmp/carl-vwidth/rv/probes, rc 0 on the installed engine too): does-ptrw.f ': MK ( n -- ) create , does> ( -- ptr width<n> ) ;  7 MK X' then 'X @ width>n' prints 7 (a width forged from an integer); does-ptrxt.f ( -- ptr xt ) and does-ptrptr.f ( -- ptr ptr u8 ) forge an xt and a pointer the same way. Acceptance: a does> clause's data-field pointee meets the storage gate (STORAGE-TERM-OK?, split out by lane 4a of habu-a-type-param-679d7eed: no width, xt, scheme or hidden term) and the raw-cell fence that refuses a pointer held in raw bytes, checked after CHECK-DOES-RUN (:26471); the three probes are refused with E-BAD-STORAGE or the raw-cell code; a clause typed ( -- ptr n ) or ( -- ptr u8 ) still loads; the docs' claims that no storage holds a width or an xt (docs/forth.md width and xt bullets, docs/forth-card.md) are true. Files: src/core/checker.f, test/compiler/width-type.f or the does> tests. Verify: the three probes; a test case each that fails on the engine before the fix; engine third generation equal; full suite. Depends: lane 4a (STORAGE-TERM-OK?). Ownership: carl. Claim: unassigned.
