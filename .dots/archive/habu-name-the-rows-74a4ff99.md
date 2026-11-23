---
title: Name the rows of an inferred effect
status: closed
priority: 2
issue-type: task
created-at: "2026-11-23T14:22:43.416601+01:00"
closed-at: "2026-11-24T16:34:34.916681+01:00"
---

Problem: `: P3 ( R [ R -- R ] -- R ) execute 1 ;` prints inferred `[ -- ] -- n` and offers `or declare ( [ -- ] -- n )`, dropping the row R: INFERRED-EFFECT (src/core/render.f:848) renders with RNAMES 0 (hunt: /tmp/carl-mmtxt-report.md). Acceptance: the inferred effect and the declare hint name every shared row, and the hinted declaration loads. Depends: none. Open (review of wnuwxvns): an effect holding one quotation twice now gets its quotation's own rows named, and the hint is rejected: `: P9 ( n [ n -- n ] -- n [ n -- n ] ) dup >r execute r> 1 ;` hints `( n [ A n -- A n | B -- B ] -- n [ A n -- A n | B -- B ] n )`, E-RETURN-STACK when declared. QREND (src/core/render.f:486) gives each visit its own owner and RPOS-NAMED? (:409) then names those tails: a diagnostic pass must record a quotation's rows on its first visit only. TEST-INFERRED-ROWS should use `( R [ R -- R ] -- R [ R -- R ] ) dup >r execute r> 1` and load its hint. Claim: unassigned (its commit is on the parked stack lmrmpkys..xvroprym above tqsnwlwz, workspace .jj-ws.noindex/carl-infname).
