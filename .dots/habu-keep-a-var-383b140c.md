---
title: "Keep a variable's declared letter in inferred"
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T13:56:47.449468+01:00"
---

Problem: a rejection names a signature variable with one letter under declared and another under inferred: ': TAKEP ( n ^a -- ) 2drop ;' then ': R40 ( ^a n -- ^a ) drop NULL-PTR TAKEP ;' notes declared '^a n -- ^a' and inferred '^b n -- ^b' (src/core/render.f INFERRED after the record's rows took a; one variable, two letters). Acceptance: inferred names a declared variable with its declared letter; a test pins R40's notes and fails first. Depends: none. Claim: unassigned.
