---
title: "Keep a variable's declared letter in inferred"
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T13:56:47.449468+01:00"
---

Problem: a rejection names a signature variable with one letter under declared and another under inferred: ': TAKEP ( n ^a -- ) 2drop ;' then ': R40 ( ^a n -- ^a ) drop NULL-PTR TAKEP ;' notes declared '^a n -- ^a' and inferred '^b n -- ^b' (src/core/render.f INFERRED after the record's rows took a; one variable, two letters). The same when a callee's row binds a declared variable: ': TWO ( ^a ^a -- n ) drop drop 0 ;' then ': S2 ( bool ^z -- n ) TWO ;' prints found 'bool ^a' and inferred 'bool ^a -- n' beside declared 'bool ^z -- n' (binding makes the callee's variable the root, and VAR-NAME-NEW, render.f:110, names a variable by its root's stored letter). Acceptance: found and inferred name a declared variable with its declared letter; a test pins R40's and S2's notes and fails first. Depends: none. Claim: unassigned.
