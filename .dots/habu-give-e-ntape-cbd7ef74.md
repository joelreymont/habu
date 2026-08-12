---
title: Give E-NTAPE-CAP a row and re-point the census fixture deliberately
status: open
priority: 2
issue-type: task
created-at: "2026-08-12T18:02:35.167215+02:00"
---

Loose end from the literal-authority landing (d994661f): the chain-census 'code no row names' fixture moved from $FF (now compilable) to a 129-row body refusing E-NTAPE-CAP - a CAPACITY, and chain-census-test-lib.f's own prose argues capacities deserve rows so they stay out of the dialect bucket. Give E-NTAPE-CAP a census row and re-point the fixture to a genuinely rowless code deliberately rather than incidentally (the comment at the fixture says to). Files: tools/chain-census*.f. Depends: none.
