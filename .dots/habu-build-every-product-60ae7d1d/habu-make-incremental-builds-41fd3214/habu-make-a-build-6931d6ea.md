---
title: "Make a build's lookups of computed names dependencies"
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T23:22:03.238688+01:00"
---

Problem: a piece's object depends only on the names its source spells and its does> clauses' names (src/habu/linker.f SPELL-TEXT, SPELL-CLAUSES, SPELL-HIT), so a lookup of a name the piece computes (search-wl, scope-find) is no dependency and a warm build can keep an object whose answer changed. Reproducer: h.f 'package QG public : FOUND ( -- n ) s" dup" get-current search-wl 0= if 0 else 1 then ; ;package', s.f 'package QS public variable Z ;package' then 'undefine dup', r.f 'QG:FOUND constant QK', main.f ': MAIN ( -- ) QK . cr ;': built cold it prints 0; with the undefine deleted it prints 1 cold and 0 warm. The same happens when the name probed is a record the build made. Acceptance: in both cases the warm build prints what the cold one does, and a test in the suite shows it. Depends: none. Claim: unassigned.
