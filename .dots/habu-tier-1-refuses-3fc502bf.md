---
title: "Tier 1 refuses a TRUSTED: does> head layout local"
status: open
priority: 3
issue-type: task
created-at: "2026-10-24T06:54:02.097138+03:00"
---

A TRUSTED: does> head that binds a layout local is refused by tier 1 while tier 0 runs it: with 'STRUCTURE pdpair FIELD a n FIELD b n ;STRUCTURE', a TRUSTED: definer whose does> head binds {: p:pdpair :} alone gives -8304 at tier 1 (tier 0 prints 3); {: a p:pdpair :} gives -8303 (tier 0 prints 5); /Users/joel/.cache/tmp/jerry-bind.quLn/pd1.f ('TRUSTED: PD-MK ( n n n -- ) {: a p:pdpair :} create ... does> ( n pdpair -- n ) @ {: b q:pdpair v :} ...') gives -8550 (tier 0 prints 8 3 1 4). A checked does> head with the same local runs at tier 1 (prints 3). Seen on the Oct 20 engine and on the jerry-bind lane (dot habu-tier-1-binds-c45e8e92), so it predates that lane; suspected cause: the widths the checker records for a trusted does> head, not proven. Found by jerry's bind lane review, 2026-10-24. Not yet worked; probe files /Users/joel/.cache/tmp/jerry-bind.quLn/rv.Ssfj/ and pd1.f (keep until worked).
