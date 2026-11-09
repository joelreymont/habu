---
title: "JSON:VALUE= is quadratic in object depth"
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T23:35:03.576793+02:00"
---

Problem: JSON:VALUE= (src/lib/json/equal.f) takes time quadratic in object nesting depth: 0.39, 1.55 and 6.11 s at 1000, 2000 and 4000 nested objects (measured 2026-11-07 when it landed). Each object level rescans its whole subtree on b's side: VALUE-SPAN-INNER (equal.f:280), MEMBER-COUNT and CLOSED-REPEATS?/READ-REPEATS? at CLOSE-MEMBERS (:315, :317), and OPEN-OBJECT plus FIND-INNER per member (:200-201). Its one caller today, Maki's OPS:REPLACE, compares shallow args objects, so no real caller is slow yet. Acceptance: b's object structure is indexed once (each member's key and value span), so the time is linear, or n log n, in the texts' size; a test at 4000 and 8000 levels whose times scale at most about twofold per doubling. Files: src/lib/json/equal.f, src/lib/json-read-test.f. Verify: hb --load src/lib/json-read-test.f. Depends: none. Ownership: equal.f. Claim: unassigned.
